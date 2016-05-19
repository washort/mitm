import "lib/monte/monte_lexer" =~ [=> makeMonteLexer :DeepFrozen]
import "lib/monte/monte_parser" =~ [=> parseExpression :DeepFrozen]
import "lib/monte/monte_expander" =~ [=> expand :DeepFrozen]

exports (mitmEval)
def FinalSlot := _makeFinalSlot.asType()

# XXX put this in safescope, we use it often enough
def zip(left :List, right :List) as DeepFrozen:
    if (left.size() != right.size()):
        throw("Can't zip lists of unequal length")
    var i := -1
    return object zip:
        to _makeIterator():
            return zip
        to next(ej):
            i += 1
            if (i == left.size()):
                throw.eject(ej, null)
            return [i, [left[i], right[i]]]

# XXX verify shadowing is done right, allowed/disallowed in proper places
# XXX verify frame handling re auditors is done right
# XXX verify BindingPatterns only accept binding objects (reified or actual)

def [auditorStampSealer :DeepFrozen,
     auditorStampUnsealer :DeepFrozen] := makeBrandPair("mitm auditor stamps")

def makeMitmEnv(localStack, outers) as DeepFrozen:
    return object mitmEnv:
        to lookup(name):
            for pile in (localStack):
                if pile.maps(name):
                    return pile[name]
            return outers[name]

        to withNames(moreNames):
            return makeMitmEnv([moreNames] + localStack, outers)

        to _uncall():
            return [makeMitmEnv, "run", [localStack, outers], [].asMap()]

def makeBinding(slot, guard) as DeepFrozen:
    return object binding:
        to get():
            return slot
        to getGuard():
            return guard

def audit(ast, audVals, frameNames, fqn, oEnv) as DeepFrozen:
    var stamps := [].asSet()
    object audition:
        to ask(auditor):
            if (!stamps.contains(auditor) && auditor.audit(audition)):
                stamps with= (auditor)

        to getGuard(name):
            if (frameNames.contains(name)):
                return oEnv.lookup(name).getGuard()
            throw(`No such binding named $name`)

        to getObjectExpr():
            return ast

        to getFQN():
            return fqn
    return stamps.asList()

def mitmAuditedBy(auditor, specimen):
    if (_auditedBy(auditor, specimen)):
        return true
    def box := specimen._sealedDispatch(auditorStampSealer)
    try:
        def stamps := auditorStampUnsealer.unseal(box)
        return stamps.contains(auditor)
    catch p:
        return false

def makeMitmObject(stamps, methods, matchers, fqn, oEnv) as DeepFrozen:
    def mitmObject
    def mitmDispatch(verb, args, kwargs):
        # Have to do this before regular dispatch because it's not overridable behavior.
        if (verb == "_sealedDispatch" && args == [auditorStampSealer]):
            return auditorStampSealer.seal(stamps)
        for m in (methods):
            var methEnv := oEnv
            if (verb == m.getVerb()):
                if (m.getPatterns().size() != args.size()):
                    throw(`${fqn}.${verb} requires ${m.getPatterns().size()} arguments, given ${args.size()}`)
                for [p, a] in (zip(m.getPatterns(), args)):
                    def newEnv := matchBind(p, a, methEnv, throw)
                    methEnv := newEnv
                for np in (m.getNamedPatterns()):
                    def dflt := np.getDefault()
                    def arg := kwargs.fetch(np.getKey(), fn {
                        if (dflt != null) {dflt} else {
                                throw(`${fqn}.${verb} requires named arg "${np.getKey()}"`)
                                }})
                    def newEnv := matchBind(np.getPattern(), arg, methEnv, throw)
                    methEnv := newEnv
                def [ResultGuard, rgEnv] := if (m.getResultGuard() == null) {
                    [Any, methEnv]
                } else {
                    _eval(m.getResultGuard(), fqn, methEnv)
                }
                def result :ResultGuard := _eval(m.getBody(), fqn, rgEnv)
                return result
        # do we have a miranda method for this verb?
        switch ([verb] + args):
            match [=="_conformTo", guard]:
                return mitmObject
            match [=="_getAllegedInterface"]:
                # XXX do the computedinterface thing
                return null
            match [=="_printOn", out]:
                out.print(`<MITM: $fqn>`)
            match [=="_respondsTo", verb, arity]:
                for m in (methods):
                    if (m.getVerb() == verb && m.getPatterns().size() == arity):
                        return true
                return (matchers.size() == 0)
            match [=="_sealedDispatch", sealer]:
                return null
            match ["_uncall"]:
                return null
            match ["_whenMoreResolved", callback]:
                callback <- (mitmObject)
                return null
            match _:
                null
        # ok so we didn't find a method that handles this
        for m in (matchers):
            escape e:
                def newEnv := matchBind(m.getPattern(), [verb, args, kwargs], oEnv, e)
                return _eval(m.getBody(), fqn, newEnv)
        throw(`Message refused: ${fqn}.${verb}/${args.size()}`)

    # The paradigmatic use case for plumbing exprs. Oh well.
    bind mitmObject:
        to _conformTo(guard):
            return mitmDispatch("_conformTo", [guard], [].asMap())
        to _getAllegedInterface():
            return mitmDispatch("_getAllegedInterface", [], [].asMap())
        to _printOn(out):
            mitmDispatch("_printOn", [], [].asMap())
        to _respondsTo(verb, arity):
            return mitmDispatch("_respondsTo", [verb, arity], [].asMap())
        to _sealedDispatch(sealer):
            return mitmDispatch("_sealedDispatch", [sealer], [].asMap())
        to _uncall():
            return mitmDispatch("_uncall", [], [].asMap())
        to _whenMoreResolved(callback):
            return mitmDispatch("_whenMoreResolved", [callback], [].asMap())
        match [verb, args, kwargs]:
            mitmDispatch(verb, args, kwargs)


def _eval(ast, fqn, env) as DeepFrozen:
    switch (ast.getNodeName()):
      match =="LiteralExpr":
          return [ast.getValue(), env]
      match =="ObjectExpr":
          def [asVal, var oEnv] := if (ast.getAsExpr() == null) {[null, env]} else {
              _eval(ast.getAsExpr(), fqn, env)}
          def audVals := [].diverge()
          if (asVal != null):
              audVals.push(asVal)
          for aud in (ast.getAuditors()):
              def [v, e] := _eval(aud, fqn, aEnv)
              audVals.push(v)
              oEnv := e
          def stamps := audit(ast, audVals, ast.getStaticScope().namesUsed(), fqn, oEnv)
          def newEnv
          def oPatt := ast.getName()
          def objName := if (oPatt.getNodeName() == "IgnorePattern") {"_"} else {
              oPatt.getNoun().getName()}
          def obj := makeMitmObject(stamps, methods, matchers, `${fqn}$$${objName}`, newEnv)
          def oGuard := if (asVal == null) {Any} else {asVal}
          bind newEnv := oEnv.withNames([oName => makeBinding(&obj, FinalSlot[oGuard])])
          return [obj, newEnv]
      match =="DefExpr":
          [null, env]
    throw("Unimplemented")


def interceptBindings(bindings) as DeepFrozen:
     return ["_auditedBy" => mitmAuditedBy] | bindings

def mitmEval(expr, outers) as DeepFrozen:
    def fullAst := parseExpression(makeMonteLexer(source, "<eval>"), astBuilder,
                               throw)
    def ast := expand(ast, astBuilder, throw)
    def patchedOuters := interceptBindings(outers)
    def env := makeMitmEnv([].asMap(), [].asMap(), patchedOuters)
    return _eval(ast, "interp", env)

