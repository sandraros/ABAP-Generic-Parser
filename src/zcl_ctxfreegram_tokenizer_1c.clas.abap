CLASS zcl_ctxfreegram_tokenizer_1c DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_ctxfreegram_tokenizer.

    CLASS-METHODS create_from_string
      IMPORTING
        string         TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_ctxfreegram_tokenizer_1c.

    CLASS-METHODS create_from_string_table
      IMPORTING
        string_table  TYPE string_table
      RETURNING
        VALUE(result) TYPE REF TO zcl_ctxfreegram_tokenizer_1c.

  PRIVATE SECTION.
    DATA type TYPE abap_typekind.
    DATA string TYPE string.
    DATA string_table TYPE string_table.
    DATA line TYPE i.
    DATA offset TYPE i.

ENDCLASS.



CLASS zcl_ctxfreegram_tokenizer_1c IMPLEMENTATION.
  METHOD create_from_string.
    result = NEW zcl_ctxfreegram_tokenizer_1c( ).
    result->type = cl_abap_typedescr=>typekind_string.
    result->string = string.
    result->offset = 0.
  ENDMETHOD.

  METHOD create_from_string_table.
    result = NEW zcl_ctxfreegram_tokenizer_1c( ).
    result->type = cl_abap_typedescr=>typekind_table.
    result->string_table = string_table.
    result->line = 1.
    result->offset = 0.
  ENDMETHOD.

  METHOD zif_ctxfreegram_tokenizer~get_next_token.
    CASE type.
      WHEN cl_abap_typedescr=>typekind_string.
        IF offset >= strlen( string ).
          RAISE EXCEPTION TYPE zcx_ctxfreegram_tokenizer
            EXPORTING
              error = zcx_ctxfreegram_tokenizer=>c_error-parse_end_of_file.
        ENDIF.
        result = lcl_token=>create( offset    = offset
                                    length    = 1
                                    character = substring( val = string
                                                           off = offset
                                                           len = 1 ) ).
        ADD 1 TO offset.
      WHEN cl_abap_typedescr=>typekind_table.
        IF line > lines( string_table )
            OR offset >= strlen( string_table[ line ] ).
          RAISE EXCEPTION TYPE zcx_ctxfreegram_tokenizer
            EXPORTING
              error = zcx_ctxfreegram_tokenizer=>c_error-parse_end_of_file.
        ENDIF.
        result = lcl_token=>create( offset    = offset
                                    length    = 1
                                    character = substring( val = string_table[ line ]
                                                           off = offset
                                                           len = 1 ) ).
        DO.
          offset = offset + 1.
          IF offset < strlen( string_table[ line ] ).
            EXIT.
          ENDIF.
          ADD 1 TO line.
          IF line > lines( string_table ).
            offset = 0.
            EXIT.
          ENDIF.
          offset = -1.
        ENDDO.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_ctxfreegram_tokenizer~is_next_token.
    CASE type.
      WHEN cl_abap_typedescr=>typekind_string.
        result = xsdbool( offset < strlen( string ) ).
      WHEN cl_abap_typedescr=>typekind_table.
        result = xsdbool( line <= lines( string_table ) AND offset < strlen( string_table[ line ] ) ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
