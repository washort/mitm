builder (exports)
def FinalSlot := _makeFinalSlot.asType()
def VarSlot := _makeVarSlot.asType()

def makeLiteralExpr(value) as DeepFrozen::
    return object literalExpr:
        to getNodeName():
            return "LiteralExpr"
        to getValue():
            return value

def makeLocalFinalNounExpr(name :Str, idx, layout) as DeepFrozen:
    return object nounExpr:
        to getNodeName():
            return "NounExpr"
        to getName():
            return name
        to getIndex():
            return idx
        to lookup(ctx):
            return ctx.local(idx)


def makeLocalSlotNounExpr(name :Str, idx, layout) as DeepFrozen:
    return object nounExpr:
        to getNodeName():
            return "NounExpr"
        to getName():
            return name
        to lookup(ctx):
            return ctx.local(idx).get()
        to assign(ctx, value):
            ctx.local(idx).put(value)

def makeFrameFinalNounExpr(name :Str, idx, layout) as DeepFrozen:
    return object nounExpr:
        to getNodeName():
            return "NounExpr"
        to getName():
            return name
        to getIndex():
            return idx
        to lookup(ctx):
            return ctx.field(idx)

def makeFrameSlotNounExpr(name :Str, idx, layout) as DeepFrozen:
    return object nounExpr:
        to getNodeName():
            return "NounExpr"
        to getName():
            return name
        to lookup(ctx):
            return ctx.field(idx).get()
        to assign(ctx, value):
            ctx.field(idx).put(value)

def makeOuterFinalNounExpr(name :Str, idx, layout) as DeepFrozen:
    return object nounExpr:
        to getNodeName():
            return "NounExpr"
        to getName():
            return name
        to getIndex():
            return idx
        to lookup(ctx):
            return ctx.outer(idx).get()

def makeOuterSlotNounExpr(name :Str, idx, layout) as DeepFrozen:
    return object nounExpr:
        to getNodeName():
            return "NounExpr"
        to getName():
            return name
        to lookup(ctx):
            return ctx.outer(idx).get()
        to assign(ctx, value):
            ctx.outer(idx).put(value)

def makeMetaContextExpr(fqnPrefix, synEnv, ast) as DeepFrozen:
    return object metaContextExpr:
        to getNodeName():
            return "MetaContextExpr"
        to getFQNPrefix():
            return fqnPrefix
        to getSynEnv():
            return synEnv
        to getAst():
            return ast

def makeMetaStateExpr() as DeepFrozen:
    return object metaStateExpr:
        to getNodeName():
            return "MetaStateExpr"

def makeBindingExpr(noun :Noun) as DeepFrozen:
    return object bindingExpr:
        to getNoun():
            return noun
        to getNodeName():
            return "BindingExpr"

def makeSeqExpr(exprs) as DeepFrozen:
    return object seqExpr:
        to getExprs():
            return exprs
        to getNodeName():
            return "SeqExpr"

def makeNamedArg(k, v) as DeepFrozen:
    return object namedArg:
        to getKey():
            return k
        to getValue():
            return v
        to getNodeName():
            return "NamedArg"

def makeMethodCallExpr(rcvr, verb, arglist, namedArgs) as DeepFrozen:
    return object methodCallExpr:
        to getReceiver():
            return rcvr
        to getVerb():
            return verb
        to getArgs():
            return arglist
        to getNamedArgs():
            return namedArgs
        to getNodeName():
            return "MethodCallExpr"

def makeVarPattern(noun, guard) as DeepFrozen:
    return object varPattern:
        to getNoun():
            return noun
        to getGuard():
            return guard
        to getBindingGuard():
            return VarSlot
        to getNodeName():
            return "VarPattern"

def makeDefExpr(pattern, exit_, expr) as DeepFrozen:
    return object defExpr:
        to getPattern():
            return pattern
        to getExit():
            return exit_
        to getExpr():
            return expr
        to getNodeName():
            return "DefExpr"

def makeAssignExpr(lvalue, rvalue) as DeepFrozen:
    return object assignExpr:
        to getLvalue():
            return lvalue
        to getRvalue():
            return rvalue
        to getNodeName():
            return "AssignExpr"

def makeMethod(docstring, verb, patterns, namedPatts, resultGuard,
               body) as DeepFrozen:
    return object ::"method":
        to getDocstring():
            return docstring
        to getVerb():
            return verb
        to getPatterns():
            return patterns
        to getNamedPatterns():
            return namedPatts
        to getResultGuard():
            return resultGuard
        to getBody():
            return body
        to getNodeName():
            return "Method"

def makeMatcher(pattern, body) as DeepFrozen:
    return object matcher:
        to getPattern():
            return pattern
        to getBody():
            return body
        to getNodeName():
            return "Matcher"

def makeScript(methods, matchers) as DeepFrozen:
    return object script:
        to getNodeName():
            return "Script"
        to getMethods():
            return methods
        to getMatchers():
            return matchers

def makeObjectExpr(origAst, docstring, name, asExpr, auditors,
                   synEnv, script) as DeepFrozen:
    return object ObjectExpr:
        to getAst():
            return origAst
        to getDocstring():
            return docstring
        to getName():
            return name
        to getAsExpr():
            return asExpr
        to getAuditors():
            return auditors
        to getSynEnv():
            return synEnv
        to getScript():
            return script
        to getNodeName():
            return "ObjectExpr"

def makeCatchExpr(body :Expr, pattern :Pattern, catcher :Expr) as DeepFrozen:
    return object catchExpr:
        to getBody():
            return body
        to getPattern():
            return pattern
        to getCatcher():
            return catcher
        to getNodeName():
            return "CatchExpr"

def makeFinallyExpr(body, unwinder) as DeepFrozen:
    return object finallyExpr:
        to getBody():
            return body
        to getUnwinder():
            return unwinder
        to getNodeName():
            return "FinallyExpr"

def makeEscapeExpr(ejectorPattern, body,
                   catchPattern, catchBody) as DeepFrozen:
    return object escapeExpr:
        to getEjectorPattern():
            return ejectorPattern
        to getBody():
            return body
        to getCatchPattern():
            return catchPattern
        to getCatchBody():
            return catchBody
        to getNodeName():
            return "EscapeExpr"

def makeIfExpr(test :Expr, consq :Expr, alt :NullOk[Expr]) as DeepFrozen:
    return object ifExpr:
        to getTest():
            return test
        to getThen():
            return consq
        to getElse():
            return alt
        to getNodeName():
            return "IfExpr"

def makeFinalPattern(noun, guard) as DeepFrozen:
    return object finalPattern:
        to getNoun():
            return noun
        to getGuard():
            return guard
        to getBindingGuard():
            return FinalSlot
        to getNodeName():
            return "FinalPattern"

def makeBindingPattern(noun) as DeepFrozen:
    return object bindingPattern:
        to getNoun():
            return noun
        to getBindingGuard():
            return Any
        to getNodeName():
            return "BindingPattern"

def makeIgnorePattern(guard]) as DeepFrozen:
    return object ignorePattern:
        to getGuard():
            return guard
        to getNodeName():
            return "IgnorePattern"

def makeListPattern(patterns) as DeepFrozen:
    return object listPattern:
        to getPatterns():
            return patterns
        to getNodeName():
            return "ListPattern"

def makeNamedParam(key, patt, default) as DeepFrozen:
    return object namedParam:
        to getKey():
            return key
        to getPattern():
            return patt
        to getDefault():
            return default
        to getNodeName():
            return "NamedParam"

object builder as DeepFrozen:
    to NounExpr(name, layout):
        return makeLocalFinalNounExpr(name)
    to BindingExpr(name, layout):
        return makeLocalFinalBindingExpr(name)
    to AssignExpr(lvalue, rvalue):
        return makeAssignExpr(lvalue, rvalue)
    to MethodCallExpr(receiver, verb, args, namedArgs):
        return makeMethodCallExpr(receiver, verb, args, namedArgs)
    to DefExpr(pattern, exit_, expr):
        return makeDefExpr(pattern, exit_, expr)
    to EscapeExpr(epatt, body, cpatt, cbody):
        return makeEscapeExpr(epatt, body, cpatt, cbody)
    to FinallyExpr(body, unwinder):
        return makeFinallyExpr(body, unwinder)
    to HideExpr(body):
        return makeHideExpr(body)
    to IfExpr(test, consq, alt):
        return makeIfExpr(test, consq, alt)
    to MetaContextExpr(fqnPrefix, synEnv, objAst):
        return makeMetaContextExpr(fqnPrefix, synEnv, objAst)
    to ObjectExpr(docstring, namePatt, asExpr, auditors, synEnv, script):
        return makeObjectExpr(docstring, namePatt, asExpr, auditors, synEnv, script)
    to Script(methods, matchers):
        return makeScript(methods, matchers)
    to Method(docstring, verb, patterns, namedParams, resultGuard, body):
        return makeMethod(docstring, verb, patterns, namedParams, resultGuard, body)
    to Matcher(pattern, body):
        return makeMatcher(pattern, body)
    to NamedParam(key, patt):
        return makeNamedParam(key, patt)
    to SeqExpr(exprs):
        return makeSeqExpr(exprs)
    to TryExpr(body, pattern, catcher):
        return makeTryExpr(body, pattern, catcher)
    to FinalPattern(noun, guard):
        return makeFinalPattern(noun, guard)
    to VarPattern(noun, guard):
        return makeVarPattern(noun, guard)
    to BindingPattern(noun, guard):
        return makeBindingPattern(noun, guard)
    to IgnorePattern(guard):
        return makeIgnorePattern(guard)
    to ListPattern(patts):
        return makeListPattern(patts)
    to ViaPattern(expr, pattern):
        return makeViaPatttern(expr, pattern)
