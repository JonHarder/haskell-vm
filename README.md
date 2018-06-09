# VM: A Virtual machine meant to run off a made up cpu architecture written in Haskell

## Instalation
* Ensure stack is [installed](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
* clone this repository
* cd into it
* install vm
  ```bash
  stack install
  ```
  
## Running it
   ```bash
   vm
   ```

## Features
* Configure a "VM" with an application (in assembly)
* Has registers to read from and write to
* Has an instruction pointer to keep track where in the application it is

## Todo
* A stack and stack pointer and base pointer
* labels
* goto
* jump
* "assembly" parsing from separate file
* an actual assembler which emits binary
* vm which understands that binary
