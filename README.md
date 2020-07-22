    project [= git] <- stack.yaml
      foo :: package <- package.yaml | foo.cabal
      bar :: package
        library    ::1 component
        executable ::  component
        test-suite ::  component
        benchmark  ::  component
          Foo :: module
          Bar :: module
          ...
        ...
      ...
