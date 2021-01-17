*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_args IMPLEMENTATION.
  METHOD arg.
    DATA l_arg TYPE string.
    l_arg = |{ arg }|.
    APPEND l_arg TO me->args.
    args = me.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_default_lexer DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_ctxfreegram_lexer.
    METHODS constructor
      IMPORTING
        text TYPE string.
  PRIVATE SECTION.
    DATA: text   TYPE string,
          offset TYPE i.
ENDCLASS.
CLASS lcl_default_lexer IMPLEMENTATION.
  METHOD constructor.
    me->text = text.
    me->offset = 0.
  ENDMETHOD.
  METHOD zif_ctxfreegram_lexer~get_next_token.
    IF offset >= strlen( text ).
      RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-parse_end_of_file.
    ENDIF.
    r_token-offset = offset.
    r_token-length = 1.
    ADD 1 TO offset.
  ENDMETHOD.
ENDCLASS.
