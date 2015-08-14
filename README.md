# haskonf

A library to configure Haskell applications with Haskell. Think XMonad and Hakyll.

To use haskonf with your application, you'll have to expose modules, requred for
configuration in your cabal file, akin to:

```
Library
	Hs-source-dirs: src
	Exposed-modules: MyCoolApp.Types
			 MyCoolApp.Core
			 MyCoolApp.Config
```

As Haskonf is a library purpose of which is to configure applications, your cabal
file — obviously — has to contain at least one `Executable` entry:

```
Executable mycoolapp
       Main-is: Main.hs
       Build-depends: base
```

Most importantly, main entry point of your application must respect possibility
of not having configuration source file, but if configuration file is to exist,
main entry point definition ought to be delegated to it. Again, see XMonad
configuration approach for a reference.

# Credits

Most of the code is shamelessly stolen from XMonad project.