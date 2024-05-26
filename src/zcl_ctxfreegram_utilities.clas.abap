CLASS zcl_ctxfreegram_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_column_width,
        component_number TYPE i,
        width            TYPE i,
      END OF ty_column_width.
    TYPES ty_column_widths TYPE STANDARD TABLE OF ty_column_width WITH EMPTY KEY.

    CLASS-METHODS get_column_widths
      IMPORTING
        table         TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE ty_column_widths.

    CLASS-METHODS render_table
      IMPORTING
        !table        TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE string .

  PRIVATE SECTION.

    TYPES ty_component_numbers TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    CLASS-METHODS get_component_numbers
      IMPORTING
        i_table                    TYPE ANY TABLE
      RETURNING
        VALUE(r_component_numbers) TYPE ty_component_numbers.
ENDCLASS.



CLASS zcl_ctxfreegram_utilities IMPLEMENTATION.


  METHOD get_column_widths.
    CONSTANTS no_of_blanks_between_columns TYPE i VALUE 2.

    DATA(component_numbers) = get_component_numbers( table ).

    LOOP AT component_numbers INTO DATA(component_number).
      DATA(max_width) = 0.
      LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
        ASSIGN COMPONENT component_number OF STRUCTURE <line> TO FIELD-SYMBOL(<comp>).
        max_width = nmax( val1 = max_width
                          val2 = strlen( |{ <comp> }| ) ).
      ENDLOOP.
      max_width = max_width + no_of_blanks_between_columns.
      INSERT VALUE #( component_number = component_number
                      width            = max_width )
          INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_component_numbers.

    DATA(line_type) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( i_table ) )->get_table_line_type( ).
    r_component_numbers  = SWITCH ty_component_numbers( line_type->type_kind
                                   WHEN cl_abap_typedescr=>typekind_struct1
                                     OR cl_abap_typedescr=>typekind_struct2
                                   THEN VALUE #( LET count_components = lines( CAST cl_abap_structdescr( line_type )->components )
                                                 IN
                                                 FOR i = 1 WHILE i <= count_components
                                                 ( i ) )
                                   ELSE VALUE #( ( 0 ) ) ).

  ENDMETHOD.


  METHOD render_table.
    DATA(column_widths) = zcl_ctxfreegram_utilities=>get_column_widths( table ).

    LOOP AT table ASSIGNING FIELD-SYMBOL(<line2>).
      DATA(line) = ``.
      LOOP AT column_widths REFERENCE INTO DATA(column_width).
        ASSIGN COMPONENT column_width->component_number OF STRUCTURE <line2> TO FIELD-SYMBOL(<comp2>).
        line = line && |{ <comp2> WIDTH = column_width->width }|.
      ENDLOOP.
      result = result && line && |\n|.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.
