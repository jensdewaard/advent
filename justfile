default:
    @just build

build:
    @stack build --ghc-options -fprint-potential-instances

profile year day:
    stack build --profile --ghc-options -fprint-potential-instances
    stack exec --profile -- advent-exe solve {{year}} {{day}} +RTS -p -hc

run year day:
    @stack run solve {{year}} {{day}}

test year day:
    @stack test
    @stack run test {{year}} {{day}}

start year day:
    @stack run start {{year}} {{day}}
