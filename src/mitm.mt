import "unittest" =~ [=> unittest]

import "lib/monte/monte_lexer" =~ [=> makeMonteLexer :DeepFrozen]
import "lib/monte/monte_parser" =~ [=> parseExpression :DeepFrozen]
import "lib/monte/monte_expander" =~ [=> expand :DeepFrozen]

import "mitm/ast" =~ [=> builder :DeepFrozen]
exports (mitmEval)

def FinalSlot := _makeFinalSlot.asType()

# XXX put these in safescope, we use 'em often enough
def FinalSlot := _makeFinalSlot.asType()
def VarSlot := _makeVarSlot.asType()
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

# Preprocessing pass to replace kernel NounExpr/BindingExpr with custom nodes
# that directly reference the storage they access, whether outer, frame, or local.

def makeScopeContour(next):
    "The boundary of a scope box."
    return object layout:
        to with(name, noun):
            return makeScopeLink(link, name, noun)
        to getPattern(name :Str):
            return null
        to requireShadowable(name :Str):
            return null
        to getFQNPrefix():
            return next.getFQNPrefix()
        to getSynEnv():
            return next.getSynEnv()
        to nestObject(name):
            return makeScopePrefixContour(layout, fqnPrefix + "$" + name)
        to nest():
            # No variables added to this contour yet, so no nesting needed.
            return layout

        to newFinal(builder, name :Str):
            return null
        to newVar(builder, name :Str):
            return null


def makeScopePrefixContour(next, prefix :Str):
    "The scope box for an object expression."
    return object prefixContour extends makeScopeContour(next):
        to getFQNPrefix():
            return prefix


def makeScopeLink(next, varName, nounExpr):
    "A single name association in a scope."
    return object link:
        to with(name, noun):
            return makeScopeLink(link, name, noun)
        to getPattern(name :Str):
            if (name == varName):
                return namePatt
            return next.getPattern(name)
        to requireShadowable(name :Str, noun):
            if (name == varName):
                throw(`$name already in scope at ${noun.getSpan()}`)
            next.requireShadowable(name, noun)
        to getFQNPrefix():
            return next.getFQNPrefix()
        to getSynEnv():
            return next.getSynEnv().with(varName, nounExpr)
        to nestObject(name :Str):
            return makeScopePrefixContour(link, fqnPrefix + "$" + name)
        to nest():
            return makeScopeContour(link)

def makeBaseContour(outers, fqnPrefix):
    "A scope box filled with existing names."
    return object layout:
        to getPattern(name :Str):
            return outers.fetch(name, fn {null})
        to requireShadowable(name :Str):
            return null
        to getFQNPrefix():
            return fqnPrefix
        to getSynEnv():
            return outers
        to nestObject(name):
            return makeScopePrefixContour(layout, fqnPrefix + "$" + name)
        to nest():
            return makeScopeContour(layout)
        to nestOuter():
            # used for REPL to add outers defined interactively.
            return null


def bindFrames(topAst, var layout) as DeepFrozen:
    "Walk over the AST, building a new one with all NounExprs replaced with specialized local/frame/outer noun references."

    var objAst := topAst
    def bindFrame(ast):
        if (ast == null):
            return null
        switch (ast.getNodeName()):
            match =="LiteralExpr":
                def value := ast.getValue()
                return ast
            match =="NounExpr":
                def name := ast.getName()
                def patt := layout.getPattern(name)
                if (patt == null):
                    throw(`Undefined variable $name at ${ast.getSpan()}`)
                return builder.NounExpr(name, layout)
            match =="BindingExpr":
                def name := ast.getNoun().getName()
                return builder.BindingExpr(name, layout)
            match =="AssignExpr":
                def targetNoun := ast.getLvalue()
                def target := targetNoun.getName()
                def rvalue := ast.getRvalue()
                def nPatt := layout.getPattern(target)
                if (nPatt != null && nPatt.getNodeName() == "FinalPattern"):
                    throw(`Can't assign to final variable $name at ${ast.getSpan()}`)
                return builder.AssignExpr(targetNoun, bindFrame(rvalue))

            match =="MethodCallExpr":
                def receiver := ast.getReceiver()
                def verb := ast.getVerb()
                def args := ast.getArgs()
                def namedArgs := [for na in (ast.getNamedArgs())
                                  builder.NamedArg(bindFrame(na.getKey()),
                                                   bindFrame(na.getValue()))]
                return builder.MethodCallExpr(
                    bindFrame(receiver), verb,
                    [for a in (args) bindFrame(a)],
                    namedArgs)
            match =="DefExpr":
                def pattern := ast.getPattern()
                def exit_ := ast.getExit()
                def expr := ast.getExpr()
                return builder.DefExpr(bindFrame(pattern), bindFrame(exit_),
                                       bindFrame(expr))
            match =="EscapeExpr":
                # XXX EoJ does elimination of unused escapes here
                def epatt := ast.getEjectorPattern()
                def body := ast.getBody()
                def cpatt := ast.getCatchPattern()
                def cbody := ast.getCatchBody()
                return builder.EscapeExpr(
                    bindFrame(epatt),
                    bindFrame(body),
                    bindFrame(cpatt),
                    bindFrame(cbody))
            match =="FinallyExpr":
                def body := ast.getBody()
                def unwinder := ast.getUnwinder()
                return builder.FinallyExpr(bindFrame(body),
                                           bindFrame(unwinder))
            match =="HideExpr":
                def body := ast.getBody()
                def outerLayout := layout
                layout := layout.nest()
                def h := builder.HideExpr(bindFrame(body))
                layout := outerLayout
                return h
            match =="IfExpr":
                #XXX EoJ eliminates unreachable branches here
                def test := ast.getTest()
                def consq := ast.getThen()
                def alt := ast.getElse()
                return builder.IfExpr(bindFrame(test), bindFrame(consq),
                                      bindFrame(alt))
            match =="MetaContextExpr":
                return builder.MetaContextExpr(layout.getFQNPrefix(), layout.getSynEnv(), objAst)
            match =="MetaStateExpr":
                return ast
            match =="ObjectExpr":
                def docstring := ast.getDocstring()
                def nounPatt := bindFrame(ast.getName())
                def name := if (noun.getNodeName() == "IgnorePattern") {"_"} else {
                    nounPatt.getNoun().getName()}
                # Push this object's ast and fqn prefix onto the stack.
                def origLayout := layout
                def origAst := objAst
                layout := layout.nestObject(name)
                objAst := ast

                # Process elements of the object expr not inside the new frame.
                def asExpr := bindFrame(ast.getAsExpr())
                def auditors := [for a in (ast.getAuditors()) bindFrame(a)]
                def script := ast.getScript()

                # Create a frame containing names referenced inside the object
                # expr.
                def used := script.getStaticScope().namesUsed()
                var fields := []
                var newSynEnv := [].asMap()
                for i in (0..!used.size()):
                    def name := used[i]
                    def namePatt := layout.getPattern(name)
                    def noun := namePatt.getNoun()
                    def newPatt := if (noun.isOuter()) {
                         namePatt
                    } else {
                         namePatt.withNounExpr(noun.asFieldAt(i))
                    }
                    fields with= (newPatt)
                    newSynEnv with= (name, newPatt)
                # Create a new scope box containing only those names.
                layout := makeBaseContour(newSynEnv, layout.getFQNPrefix())

                # Transform methods and matchers to refer to the new scope.
                var methods := []
                var matchers := []
                for m in (script.getMethods()):
                    def objLayout := layout
                    layout := layout.nest()
                    methods with= (builder.Method(
                        m.getDocstring(),
                        m.getVerb(),
                        [for p in (m.getPatterns()) bindFrame(p)],
                        [for np in (m.getNamedPatterns())
                         builder.NamedParam(bindFrame(np.getKey()),
                                            bindFrame(np.getPattern()),
                                            bindFrame(np.getDefault()))],
                        bindFrame(m.getResultGuard()),
                        bindFrame(m.getBody()),
                        ))
                    layout := objLayout
                for m in (ast.getScript().getMatchers()):
                    def objLayout := layout
                    layout := layout.nest()
                    matchers with= (builder.Matcher(
                        bindFrame(m.getPattern()),
                        bindFrame(m.getBody())))
                    layout := objLayout
                def obj := builder.ObjectExpr(
                    objAst, docstring, nounPatt, asExpr, auditors,
                    newSynEnv, builder.Script(methods, matchers))
                layout := origLayout
                ast := origAst
                return obj
           match =="SeqExpr":
               def exprs := ast.getExprs()
               return builder.SeqExpr([for e in (exprs) bindFrame(e)])
           match =="TryExpr":
               def body := ast.getBody()
               def pattern := ast.getPattern()
               def catcher := ast.getCatcher()
               return builder.TryExpr(bindFrame(body), bindFrame(pattern),
                                      bindFrame(catcher))

           match =="FinalPattern":
               def guard := ast.getGuard()
               def noun := ast.getNoun()
               def name := noun.getName()
               layout.requireShadowable(name, noun)
               def new := builder.FinalPattern(
                   layout.newFinal(builder, name),
                   bindFrame(guard),
                   layout)
               layout with= (name, new)
               return new
           match =="VarPattern":
               def guard := ast.getGuard()
               def noun := ast.getNoun()
               def name := noun.getName()
               layout.requireShadowable(name, noun)
               def new := builder.VarPattern(
                   layout.newVar(builder, name),
                   bindFrame(guard),
                   layout)
               layout with= (name, new)
               return new
           match =="BindingPattern":
               def noun := ast.getNoun()
               def name := noun.getName()
               layout.requireShadowable(name, noun)
               def new := builder.BindingPattern(
                   layout.newVar(builder, name),
                   layout)
               layout with= (name, new)
               return new
           match =="IgnorePattern":
               return builder.IgnorePattern(bindFrame(ast.getGuard()))
           match =="ListPattern":
               def patterns := ast.getPatterns()
               return builder.ListPattern([for p in (patterns)
                                           bindFrame(p)])
           match =="ViaPattern":
               def expr := ast.getExpr()
               def pattern := ast.getPattern()
               return builder.ViaPattern(bindFrame(expr),
                                         bindFrame(pattern))
    return bindFrame(topAst)


def patternFromGuard(n, i, g):
    switch (g):
       match via (FinalSlot.extract) subguard:
           return builder.FinalPattern(builder.OuterFinalNounExpr(n, i),
                                       builder.LiteralExpr(subguard))
       match via (VarSlot.extract) subguard:
           return builder.VarPattern(builder.OuterSlotNounExpr(n, i),
                                       builder.LiteralExpr(subguard))
       match _:
           return builder.BindingPattern(builder.OuterSlotNounExpr(n, i))


def makeBinding(slot, guard) as DeepFrozen:
    return object binding:
        to get():
            return slot
        to getGuard():
            return guard

def audit(ast, audVals, oSynEnv, fqn, oCtx) as DeepFrozen:
    var stamps := [].asSet()
    object audition:
        to ask(auditor):
            if (!stamps.contains(auditor) && auditor.audit(audition)):
                stamps with= (auditor)

        to getGuard(name):
            if (oSynEnv.contains(name)):
                return oCtx.getFieldGuard(oSynEnv[name].getIndex())
            throw(`No such binding named $name`)

        to getObjectExpr():
            return ast

        to getFQN():
            return fqn
    return stamps.asList()

def mitmAuditedBy(auditor, specimen) as DeepFrozen:
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

    return mitmObject

def maybeDeslotify(b):
    # It's not enough to have a FinalSlot in the binding, it has to be guarded
    # as such.
    if (FinalSlot.supersetOf(b.getGuard()):
        return b.get().get()
    # Everything else is kept as a slot.
    return b.get()

object makeEvalContext as DeepFrozen:
    to run(localVals, localGuards, fieldVals, fieldGuards,
           outerVals, outerGuards) as DeepFrozen:
    return object evalContext:
        to local(i):
            return localVals[i]
        to field(i):
            return fielsVals[i]
        to getFieldGuard(i):
            return fieldGuards[i]
        to outer(i):
            return outerVals[i]

    to fromBindings(localBindings, fieldBindings, outerBindings):
        return makeEvalContext(
            [for b in (localBindings) maybeDeslotify(b)],
            [for b in (localBindings) b.getGuard()],
            [for b in (fieldBindings) maybeDeslotify(b)],
            [for b in (fieldBindings) b.getGuard()],
            [for b in (outerBindings) maybeDeslotify(b)],
            [for b in (outerBindings) b.getGuard()])

def _eval(ast, ctx) as DeepFrozen:
    switch (ast.getNodeName()):
      match =="LiteralExpr":
          return [ast.getValue(), ctx]
      match =="ObjectExpr":
          def [asVal, var oCtx] := if (ast.getAsExpr() == null) {[null, env]} else {
              _eval(ast.getAsExpr(), ctx)}
          def audVals := [].diverge()
          if (asVal != null):
              audVals.push(asVal)
          for aud in (ast.getAuditors()):
              def [v, e] := _eval(aud, oCx)
              audVals.push(v)
              oCtx := e
          def stamps := audit(ast.getAst(), audVals,
                              ast.getSynEnv(), fqn, oCtx)
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
     return ["_auditedBy" => &&mitmAuditedBy] | bindings

def mitmEval(expr, origOuters) as DeepFrozen:
    def fullAst := parseExpression(makeMonteLexer(source, "<eval>"), astBuilder,
                               throw)
    def kernelAst := expand(fullast, astBuilder, throw)
    def outers := interceptBindings(origOuters)
    def synEnv := [for i => k in (outers.getKeys())
                   k => patternFromGuard(k, i, outers[k].getGuard())]
    def layout := makeBaseContour(synEnv, "<eval>")
    def ast := bindFrames(kernelAst, layout)
    def ctx := makeEvalContext.fromBindings([], [], outers.getValues())
    return _eval(ast, ctx)


def testLiteral(assert):
    assert.equal(eval("1", safeScope), 1)

unittest([testLiteral])
