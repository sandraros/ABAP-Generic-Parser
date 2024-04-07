CLASS zcx_ctxfreegram_tokenizer DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ENUM ty_error STRUCTURE c_error,
             parse_end_of_file,
           END OF ENUM ty_error STRUCTURE c_error.

    METHODS constructor
      IMPORTING
        !error    TYPE ty_error
        !previous TYPE REF TO cx_root OPTIONAL
        msgv1     TYPE symsgv OPTIONAL
        msgv2     TYPE symsgv OPTIONAL.

    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.

    DATA error TYPE ty_error READ-ONLY.
    DATA msgv1 TYPE symsgv READ-ONLY.
    DATA msgv2 TYPE symsgv READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA lr_parser          TYPE REF TO zcl_ctxfreegram_lr_parser.
    DATA action_goto_tables TYPE string.
    DATA rules              TYPE string.

ENDCLASS.



CLASS zcx_ctxfreegram_tokenizer IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->error = error.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
  ENDMETHOD.


  METHOD get_text.
    CASE error.
      WHEN c_error-parse_end_of_file.
        result = 'End of file'(010).
    ENDCASE.
    replace '&1' in result with msgv1.
    replace '&2' in result with msgv2.
  ENDMETHOD.

  METHOD get_longtext.
    result = get_text( ).
  ENDMETHOD.

ENDCLASS.
