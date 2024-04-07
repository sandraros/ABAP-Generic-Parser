"! A lexer is like a parser but whose input tokens are characters, and
"! output is a list of tokens (tokenizer interface) instead of AST.
CLASS zcl_ctxfreegram_lexer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_ctxfreegram_tokenizer.

    DATA parser TYPE REF TO zcl_ctxfreegram_lr_parser READ-ONLY.

    CLASS-METHODS create
      IMPORTING
        io_context_free_grammar TYPE REF TO zcl_ctxfreegram_grammar
        record_trace            TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)           TYPE REF TO zcl_ctxfreegram_lexer.

  PRIVATE SECTION.

    data token_number type i.

ENDCLASS.


CLASS zcl_ctxfreegram_lexer IMPLEMENTATION.

  METHOD create.

    result = NEW zcl_ctxfreegram_lexer( ).
    result->parser = zcl_ctxfreegram_lr_parser=>create(
                       io_context_free_grammar = io_context_free_grammar
                       record_trace            = record_trace ).
*    DATA(ast) = result->parser->get_ast( ).
*    DATA(dfs_iterator) = ast->get_dfs_iterator( ).
    result->token_number = 0.

  ENDMETHOD.

  METHOD zif_ctxfreegram_tokenizer~get_next_token.

    if token_number = 0.
      parser->parse( ).
    endif.

    token_number = token_number + 1.

*    DATA(result) = ast->get_next( ).

  ENDMETHOD.

  METHOD zif_ctxfreegram_tokenizer~is_next_token.

  ENDMETHOD.

ENDCLASS.
