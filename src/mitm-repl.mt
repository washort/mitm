import "lib/json" =~ [=> JSON :DeepFrozen]
import "lib/tubes" =~ [
    => makeUTF8DecodePump :DeepFrozen,
    => makeUTF8EncodePump :DeepFrozen,
    => makePumpTube :DeepFrozen,
]
import "fun/repl" =~ [=> makeREPLTube :DeepFrozen]
import "lib/help" =~ [=> help :DeepFrozen]
import "mitm" =~ [=> mitmEval :DeepFrozen]
exports (main)


def makeMonteParser(&environment, unsealException) as DeepFrozen:
    var failure :NullOk[Str] := null
    var result := null

    return object monteEvalParser:
        to getFailure() :NullOk[Str]:
            return failure

        to failed() :Bool:
            return failure != null

        to finished() :Bool:
            return true

        to results() :List:
            return [result]

        to feed(token):
            monteEvalParser.feedMany([token])

        to reset():
            failure := null
            result := null
            return monteEvalParser

        to feedMany(tokens):
            try:
                def [val, newEnv] := mitmEval(tokens, environment)
                result := val
                # Preserve side-effected new stuff from e.g. playWith.
                environment := newEnv | environment
            catch via (unsealException) [problem, trail]:
                failure := `$problem`
                # Discard the first line from the trail since it's always the
                # eval() frame, which is noisy and useless. Also the second
                # line. And maybe more lines in the future?
                for line in (trail.reverse().slice(2)):
                    traceln(line)

def reduce(result) as DeepFrozen:
    return result

def main(argv, => Timer, => currentProcess, => currentRuntime, => currentVat,
         => getAddrInfo, # => packageLoader,
         => makeFileResource, => makeProcess,
         => makeStdErr, => makeStdIn, => makeStdOut,
         => makeTCP4ClientEndpoint, => makeTCP4ServerEndpoint,
         => unsealException, => unsafeScope) as DeepFrozen:

    var environment := safeScope | [
        # Typhon unsafe scope.
        => &&Timer, => &&currentProcess, => &&currentRuntime, => &&currentVat,
        => &&getAddrInfo,
        => &&makeFileResource, => &&makeProcess, => &&makeStdErr, => &&makeStdIn,
        => &&makeStdOut, => &&makeTCP4ClientEndpoint, => &&makeTCP4ServerEndpoint,
        => &&unsealException,
        # REPL-only fun.
        => &&JSON, => &&help, # => &&playWith,
    ]

    def stdin := makeStdIn() <- flowTo(makePumpTube(makeUTF8DecodePump()))
    def stdout := makePumpTube(makeUTF8EncodePump())
    stdout <- flowTo(makeStdOut())
    def parser := makeMonteParser(&environment, unsealException)
    def replTube := makeREPLTube(fn {parser.reset()}, reduce,
                                 "▲> ", "…> ", stdout)
    stdin <- flowTo(replTube)

    return 0

