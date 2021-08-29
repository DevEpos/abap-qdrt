*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF lty_select_field,
    value TYPE string,
    alias TYPE string,
  END OF lty_select_field,
  lty_select_fields TYPE STANDARD TABLE OF lty_select_field WITH EMPTY KEY,

  BEGIN OF lty_select_part,
    fields TYPE lty_select_fields,
  END OF lty_select_part,
  BEGIN OF lty_where_comp,
    value  TYPE string,
    and_or TYPE string,
  END OF lty_where_comp,
  lty_where_comps TYPE STANDARD TABLE OF lty_where_comp WITH EMPTY KEY,

  BEGIN OF lty_where_part,
    components TYPE lty_where_comps,
  END OF lty_where_part,

  BEGIN OF lty_query,
    select_part TYPE lty_select_part,
    where_part  TYPE lty_where_part,
  END OF lty_query.


INTERFACE lif_statement_parser.
  METHODS parse
    RETURNING
      VALUE(result) TYPE REF TO data.
ENDINTERFACE.

CLASS lcl_token_parser DEFINITION
 ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          tokens TYPE zcl_qdrt_sql_query_parser=>ty_tokens.
  PROTECTED SECTION.
    DATA:
      tokens        TYPE zcl_qdrt_sql_query_parser=>ty_tokens,
      token_count   TYPE i,
      current_index TYPE i,
      current_token TYPE zcl_qdrt_sql_query_parser=>ty_token.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Navigate to next token in the list</p>
      "!
      next_token,
      "! <p class="shorttext synchronized" lang="en">Delete the current token</p>
      "!
      delete_current,
      "! <p class="shorttext synchronized" lang="en">Set index to first token in the list</p>
      "!
      set_index_to_first,
      "! <p class="shorttext synchronized" lang="en">Navigate to the previous token in the list</p>
      "!
      previous_token,
      "! <p class="shorttext synchronized" lang="en">Retrieve the next token for the given value</p>
      "!
      get_token
        IMPORTING
          token              TYPE string
          start_from_current TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(exists)      TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Checks if there is another token after the current one</p>
      "!
      has_next_token
        RETURNING
          VALUE(result) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Checks if there is another token before the current one</p>
      "!
      has_previous_token
        RETURNING
          VALUE(result) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Check if the next token has the given value</p>
      "!
      is_next_token
        IMPORTING
          next_token    TYPE string
        RETURNING
          VALUE(result) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Check if the previous token has the given value</p>
      "!
      is_previous_token
        IMPORTING
          previous_token TYPE string
        RETURNING
          VALUE(result)  TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Updates the current token from the current working structure</p>
      "!
      update_from_current,
      "! <p class="shorttext synchronized" lang="en">Deletes the next token in the list</p>
      "!
      delete_next,
      "! <p class="shorttext synchronized" lang="en">Delete the previous token in the list</p>
      "!
      delete_previous,

      "! <p class="shorttext synchronized" lang="en">Checks if the token matches a token in a comma separated token list</p>
      "!
      token_matches
        IMPORTING
          check_list     TYPE string
          token_to_check TYPE string
        RETURNING
          VALUE(result)  TYPE abap_bool.
ENDCLASS.

CLASS lcl_query_token_simplifier DEFINITION
INHERITING FROM lcl_token_parser.

  PUBLIC SECTION.
    METHODS simplify
      RETURNING
        VALUE(rt_tokens) TYPE zcl_qdrt_sql_query_parser=>ty_tokens.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      simplify_by_clause
        IMPORTING
          clause     TYPE string
          simplified TYPE string,
      simplify_joins,
      simplify_conditions.
ENDCLASS.

CLASS lcl_query_param_parser DEFINITION
 INHERITING FROM lcl_token_parser.

  PUBLIC SECTION.
    INTERFACES lif_statement_parser.
    ALIASES parse
      FOR lif_statement_parser~parse.

  PRIVATE SECTION.
    DATA:
      param TYPE REF TO zcl_qdrt_sql_query_parser=>ty_parameter.
    METHODS:
      parse_type,
      parse_length,
      parse_value,
      parse_decimals,
      parse_name.
ENDCLASS.
