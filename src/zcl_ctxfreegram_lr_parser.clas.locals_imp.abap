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

CLASS lcl_token_end_of_input IMPLEMENTATION.

  METHOD create.
    result = NEW lcl_token_end_of_input( ).
    result->text   = text.
  ENDMETHOD.

  METHOD zif_ctxfreegram_token~get_text.
    result = text.
  ENDMETHOD.

  METHOD zif_ctxfreegram_token~get_detailed_info.
    result = 'End of input'.
  ENDMETHOD.

ENDCLASS.
