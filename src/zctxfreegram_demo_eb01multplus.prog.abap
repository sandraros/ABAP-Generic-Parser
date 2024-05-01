*&---------------------------------------------------------------------*
*& Report zctxfreegram_demo
*&---------------------------------------------------------------------*
*&
*& https://en.wikipedia.org/wiki/LR_parser
*& |__ Additional example 1 + 1
*&
*& retrieved May 1st, 2024
*&
*& (1) E → E * B
*& (2) E → E + B
*& (3) E → B
*& (4) B → 0
*& (5) B → 1
*&
*&---------------------------------------------------------------------*
REPORT zctxfreegram_demo_eb01multplus.

CLASS lcl_main DEFINITION DEFERRED.

PARAMETERS p_text TYPE string LOWER CASE DEFAULT '1+1'.

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

    TYPES:
        BEGIN OF ts_symbol_tree,
          level TYPE i,
          symbol type ref to zif_ctxfreegram_parsed_symbol,
        END OF ts_symbol_tree.
        TYPES tt_symbol_tree TYPE STANDARD TABLE OF ts_symbol_tree WITH EMPTY KEY.

    DATA(symbol_tree) = VALUE tt_symbol_tree( ( level  = 0
                                                symbol = lexer->parser->symbol_stack[ 1 ] ) ).
    LOOP AT symbol_tree REFERENCE INTO DATA(node_of_symbol_tree).
      DATA(tabix) = sy-tabix.
      CASE TYPE OF node_of_symbol_tree->symbol.
        WHEN TYPE zcl_ctxfreegram_parsed_term.
          DATA(parsed_term) = CAST zcl_ctxfreegram_parsed_term( node_of_symbol_tree->symbol ).
          DATA(indent) = 1 + ( 4 * node_of_symbol_tree->level ).
          WRITE : AT /indent |Offset { parsed_term->token->offset }, length { parsed_term->token->length }: "{ parsed_term->token->get_text( ) }"|.
        WHEN TYPE zcl_ctxfreegram_parsed_nonterm.
          DATA(parsed_nonterm) = CAST zcl_ctxfreegram_parsed_nonterm( node_of_symbol_tree->symbol ).
          DATA(rule) = grammar->formatted_rules[ index = parsed_NONterm->rule_number ].
          indent = 1 + ( 4 * node_of_symbol_tree->level ).
          WRITE : AT /indent |{ rule-plain_text } (#{ parsed_NONterm->rule_number }) :|.
          DATA(tabix_2) = tabix.
          LOOP AT parsed_nonterm->child_symbols INTO DATA(parsed_symbol_2).
            tabix_2 = tabix_2 + 1.
            INSERT VALUE #( level  = node_of_symbol_tree->level + 1
                            symbol = parsed_symbol_2 )
                INTO symbol_tree
                INDEX tabix_2.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

*    DATA(parser) = zcl_ctxfreegram_lr_parser=>create( io_context_free_grammar = grammar ).
*
*    DATA(ast) = parser->parse( lexer ).

  ENDMETHOD.
ENDCLASS.
