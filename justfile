default:
    @just build

build:
    @stack build --ghc-options -fprint-potential-instances

run year day:
    @stack run {{year}} {{day}}

test:
    @stack test
