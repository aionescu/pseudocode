# pseudocode

Pseudocode compiler for .NET

## Examples

You can find some test programs in the [Examples](Examples) folder.

## Building & Running

You will need the .NET SDK, which can be downloaded [here](https://dotnet.microsoft.com/download).

To build, run `dotnet build` in the repo's root directory.

To run the compiler, run `dotnet run <file>` (e.g. `dotnet run Examples/Factorial.psc`). This will also build the project if it was not built beforehand.

## VS Code Extension

This repo also includes a VS Code extension for Pseudocode syntax highlighting.

You can install it by running the `install-vscode-ext.sh` script, or manually copying/symlinking the `pseudocode-vscode` folder into your VS Code extensions folder.

## License

This repository is licensed under the terms of the GNU General Public License v3.
For more details, see [the license file](LICENSE.txt).
