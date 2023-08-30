default:
    @just --list

build:
    @stack build

run year day:
    @stack run {{year}} {{day}}

test:
    @stack test