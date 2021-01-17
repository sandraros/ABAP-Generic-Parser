*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

class lcl_args definition.
  public section.
    methods arg importing VALUE(arg) TYPE simple RETURNING VALUE(args) TYPE REF TO lcl_args.
    DATA args TYPE TABLE OF string.
endclass.
