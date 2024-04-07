*&---------------------------------------------------------------------*
*& Report zctxfreegram_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zctxfreegram_demo.

CLASS lcl_main DEFINITION DEFERRED.

PARAMETERS p_text TYPE string LOWER CASE DEFAULT '1+0'.

START-OF-SELECTION.
  DATA main TYPE REF TO lcl_main.

  TRY.

      CALL METHOD ('LCL_MAIN')=>create RECEIVING result = main.

      CALL METHOD main->('RUN').

    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

CLASS lcl_main DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lcl_main.

    METHODS run
      RAISING
        zcx_ctxfreegram.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_main( ).
  ENDMETHOD.

  METHOD run.
    " start: E
    " E: E '*' B
    " E: E '+' B
    " E: B
    " B: '0'
    " B: '1'
    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(e) = factory->new_nonterminal( 'E' ).
    DATA(b) = factory->new_nonterminal( 'B' ).
    DATA(star) = factory->new_terminal( '*' ).
    DATA(plus) = factory->new_terminal( '+' ).
    DATA(_0) = factory->new_terminal( '0' ).
    DATA(_1) = factory->new_terminal( '1' ).

    DATA(grammar) = zcl_ctxfreegram_grammar=>create(
        start_rule = start
        rules      = VALUE #(
            ( lhs = start rhs = e )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( star ) ( b ) ) ) )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( plus ) ( b ) ) ) )
            ( lhs = e     rhs = b )
            ( lhs = b     rhs = _0 )
            ( lhs = b     rhs = _1 ) ) ).

    DATA(lexer) = zcl_ctxfreegram_lexer=>create( io_context_free_grammar = grammar ).

    DATA(tokenizer) = zcl_ctxfreegram_tokenizer_1c=>create_from_string( p_text ).

    lexer->parser->parse( tokenizer ).

*    DATA(parser) = zcl_ctxfreegram_lr_parser=>create( io_context_free_grammar = grammar ).
*
*    DATA(ast) = parser->parse( lexer ).

  ENDMETHOD.
ENDCLASS.
