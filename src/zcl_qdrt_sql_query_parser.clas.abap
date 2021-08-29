"! <p class="shorttext synchronized" lang="en">Parser for SQL Query</p>
CLASS zcl_qdrt_sql_query_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_parameter.
        INCLUDE TYPE zif_qdrt_ty_global=>ty_sql_query_param.
    TYPES is_used TYPE abap_bool.
    TYPES line_in_query TYPE i.
    TYPES: END OF ty_parameter.
    TYPES: ty_parameters TYPE STANDARD TABLE OF ty_parameter WITH KEY name.

    TYPES: ty_sql_statement_type TYPE c LENGTH 1.
    TYPES:
      BEGIN OF ty_token,
        value             TYPE string,
        value_no_modifier TYPE string,
        type              TYPE token_type,
        row               TYPE i,
        col               TYPE i,
      END OF ty_token.
    TYPES: ty_tokens TYPE STANDARD TABLE OF ty_token WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_statement,
        first_token   TYPE string,
        is_main_query TYPE abap_bool,
        terminator    TYPE stmnt_term,
        type          TYPE stmnt_type,
        tokens        TYPE ty_tokens,
      END OF ty_statement.
    TYPES: ty_statements TYPE STANDARD TABLE OF ty_statement WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF c_keywords,
        select           TYPE string VALUE 'SELECT',
        count            TYPE string VALUE 'COUNT',
        count_pattern    TYPE string VALUE 'COUNT(*',
        count_star       TYPE string VALUE 'COUNT(*)',
        data             TYPE string VALUE 'DATA',
        where            TYPE string VALUE 'WHERE',
        fields           TYPE string VALUE 'FIELDS',
        order_by         TYPE string VALUE 'ORDER BY',
        group_by         TYPE string VALUE 'GROUP BY',
        with             TYPE string VALUE 'WITH',
        inner_join       TYPE string VALUE 'INNER JOIN',
        left_outer_join  TYPE string VALUE 'LEFT OUTER JOIN',
        right_outer_join TYPE string VALUE 'RIGHT OUTER JOIN',
        union            TYPE string VALUE 'UNION',
        cross_join       TYPE string VALUE 'CROSS JOIN',
      END OF c_keywords.

    CONSTANTS:
      BEGIN OF c_token_class,
        join TYPE i VALUE 2,
      END OF c_token_class.
    CONSTANTS:
      BEGIN OF c_literals,
        star_value TYPE string VALUE '*',
      END OF c_literals.
    CONSTANTS:
      BEGIN OF c_statement_type,
        computation  TYPE stmnt_type VALUE 'C',
        keyword      TYPE stmnt_type VALUE 'K',
        line_comment TYPE stmnt_type VALUE 'P',
        comment      TYPE stmnt_type VALUE 'S',
        unknown      TYPE stmnt_type VALUE 'U',
      END OF c_statement_type.

    CLASS-METHODS:
      class_constructor,
      "! <p class="shorttext synchronized" lang="en">Extracts entities from given query</p>
      get_entities_in_query
        IMPORTING
          query         TYPE string
        RETURNING
          VALUE(result) TYPE zif_qdrt_ty_global=>ty_tabname_range.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Create new parser</p>
      constructor
        IMPORTING
          query TYPE string,
      "! <p class="shorttext synchronized" lang="en">Parse the query</p>
      parse
        RETURNING
          VALUE(query) TYPE REF TO zcl_qdrt_sql_query
        RAISING
          zcx_qdrt_sql_query_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      invalid_keyword_range  TYPE RANGE OF string,
      aggregate_func_range   TYPE RANGE OF string,
      single_result_keywords TYPE RANGE OF string.

    DATA:
      raw_query               TYPE string,
      query_type              TYPE string,
      executable_query        TYPE string,
      select_query_end_offset TYPE i,
      sql_query               TYPE REF TO zcl_qdrt_sql_query,
      stmnts                  TYPE ty_statements,
      select_stmnt            TYPE ty_statement,
      parameters              TYPE ty_parameters,
      query_entities          TYPE zif_qdrt_ty_global=>ty_tabname_range,
      select_stmnt_index      TYPE i,
      stmnts_raw              TYPE sstmnt_tab,
      tokens_raw              TYPE stokesx_tab,
      query_lines             TYPE STANDARD TABLE OF string,
      select_query_end_row    TYPE i,
      is_single_result        TYPE abap_bool.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Tokenize the query statement</p>
      tokenize
        RAISING
          zcx_qdrt_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Process the tokens to get details of the query</p>
      parse_query
        RAISING
          zcx_qdrt_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Do some prelimary validation on the query</p>
      pre_validate_statements
        RAISING
          zcx_qdrt_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Check for invalid tokens before hand</p>
      pre_validate_tokens
        RAISING
          zcx_qdrt_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Combines tokens with statements for easier processing</p>
      combine_stmnt_with_tokens,
      "! <p class="shorttext synchronized" lang="en">Checks ABAP syntax of query</p>
      "!
      check_syntax
        RAISING
          zcx_qdrt_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Insert INTO TABLE clause into select stmnt</p>
      insert_into_table_clause
        CHANGING
          query_lines TYPE string_table
        RAISING
          zcx_qdrt_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Extract parameters from query string</p>
      extract_parameters,
      "! <p class="shorttext synchronized" lang="en">Check if all parameters are used in query</p>
      check_parameters_where_used,
      "! <p class="shorttext synchronized" lang="en">Simplifation of some tokens</p>
      "! Parses tokens in SELECT/WITH clause adn combines them into one token where
      "! it makes sense.<br>
      "! Example: <br>
      "! Token 'ORDER' and token 'BY' will be combined into Token 'ORDER BY'
      simplify_tokens,
      "! <p class="shorttext synchronized" lang="en">Determines the properties of the main statement in the query</p>
      "! This are needed to properly create the subroutine program for the data
      "! selection
      determine_main_stmnt_props.
ENDCLASS.



CLASS zcl_qdrt_sql_query_parser IMPLEMENTATION.

  METHOD class_constructor.
    invalid_keyword_range = VALUE #(
      LET i = 'I' eq = 'EQ' IN
      ( sign = i option = eq low = `INSERT` )
      ( sign = i option = eq low = `MODIFY` )
      ( sign = i option = eq low = `DELETE` )
      ( sign = i option = eq low = `UP` )
      ( sign = i option = eq low = `UPDATE` )
      ( sign = i option = eq low = `INTO` )
      ( sign = i option = eq low = `REF` )
      ( sign = i option = eq low = `ENDSELECT` )
      ( sign = i option = eq low = `ENDWITH` )
      ( sign = i option = eq low = `READ` )
      ( sign = i option = eq low = `LOOP` )
      ( sign = i option = eq low = `FIELD-SYMBOLS` )
      ( sign = i option = eq low = `ASSIGN` ) ).

    single_result_keywords = VALUE #(
      ( sign = 'I' option = 'CP' low = 'COUNT*' )
      ( sign = 'I' option = 'EQ' low = 'SUM' )
      ( sign = 'I' option = 'EQ' low = 'MAX' )
      ( sign = 'I' option = 'EQ' low = 'MIN' )
      ( sign = 'I' option = 'EQ' low = 'AVG' ) ).

    aggregate_func_range = VALUE #(
      ( sign = 'I' option = 'EQ' low = 'GROUP' ) ).
  ENDMETHOD.

  METHOD get_entities_in_query.
    CHECK query IS NOT INITIAL.
    DATA(parser) = NEW zcl_qdrt_sql_query_parser( query ).
    TRY.
        parser->tokenize( ).
        result = parser->query_entities.
      CATCH zcx_qdrt_sql_query_error.
    ENDTRY.

  ENDMETHOD.

  METHOD constructor.
    raw_query = query.
    SPLIT raw_query AT cl_abap_char_utilities=>cr_lf INTO TABLE query_lines.
  ENDMETHOD.


  METHOD parse.
    CHECK raw_query IS NOT INITIAL.

    tokenize( ).
    pre_validate_tokens( ).
    pre_validate_statements( ).

    combine_stmnt_with_tokens( ).

*.. Check the type of the query
    determine_main_stmnt_props( ).

    check_syntax( ).

    extract_parameters( ).
    check_parameters_where_used( ).

*.. Syntax check is done, now the actual parsing/tokenization
*.. of the query is performed
    IF lines( stmnts ) = 1.
      select_stmnt = stmnts[ 1 ].
      CLEAR stmnts.

      simplify_tokens( ).
      parse_query( ).
    ELSEIF stmnts IS INITIAL.
      RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error
        EXPORTING
          textid = zcx_qdrt_sql_query_error=>no_select_statement.
    ENDIF.

*.. Parse without error
    sql_query = NEW zcl_qdrt_sql_query(
      query_data = VALUE #(
        source                   = raw_query
        select_source            = executable_query
        last_row_in_select_stmnt = select_query_end_row
        last_row_offset          = select_query_end_offset
        main_select_stmnt_type   = query_type
        is_single_result_query   = is_single_result
        db_entities              = query_entities )
      parameters = VALUE #( FOR param IN parameters WHERE ( is_used = abap_true ) ( CORRESPONDING #( param ) ) ) ).

    query = sql_query.

  ENDMETHOD.

  METHOD check_parameters_where_used.
    CHECK parameters IS NOT INITIAL.

    ASSIGN stmnts[ is_main_query = abap_true ] TO FIELD-SYMBOL(<select>).
    CHECK sy-subrc = 0.

    LOOP AT <select>-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE type = sana_tok_field.
      LOOP AT parameters ASSIGNING FIELD-SYMBOL(<parameter>) WHERE is_used = abap_false.
        IF <token>-value CP '*' && <parameter>-name  && '*'.
          <parameter>-is_used = abap_true.
        ENDIF.
      ENDLOOP.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF line_exists( parameters[ is_used = abap_false ] ).
***      IF mf_fill_log_for_msg = abap_true.
***        DATA(lo_protocol) = zcl_uitb_protocol=>get_instance( ).
***        LOOP AT parameters ASSIGNING <ls_parameter> WHERE is_used = abap_false.
***          lo_protocol->add_warning(
***              iv_message     = |Parameter { <ls_parameter>-name } is not used in query|
***              iv_line_number = CONV #( <ls_parameter>-line_in_query )
***          ).
***        ENDLOOP.
***      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_syntax.
    DATA: line    TYPE i,
          word    TYPE string,
          message TYPE string.

    SELECT SINGLE * FROM trdir
    INTO @DATA(dir)
    WHERE name = @sy-cprog.

    DATA(l_query_lines) = query_lines.

    insert_into_table_clause( CHANGING query_lines = l_query_lines  ).

    DATA(source_code) = VALUE string_table(
      ( |REPORT ZCHECK_QUERY.| )
      ( LINES OF l_query_lines )
    ).

    SYNTAX-CHECK FOR source_code MESSAGE         message
                                 LINE            line
                                 WORD            word
                                 DIRECTORY ENTRY dir.

    IF message IS NOT INITIAL AND sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error
        EXPORTING
          text        = message
          line_number = line - 1.
    ENDIF.

*.. Remove all lines but the select statement
    IF select_stmnt_index > 1.
      DELETE query_lines FROM 1 TO select_stmnt_index - 1.
      select_query_end_row = select_query_end_row - select_stmnt_index + 1.
    ENDIF.

    CONCATENATE LINES OF query_lines INTO executable_query SEPARATED BY cl_abap_char_utilities=>cr_lf.

  ENDMETHOD.

  METHOD combine_stmnt_with_tokens.
*.. Combine statements and tokens
    LOOP AT stmnts_raw ASSIGNING FIELD-SYMBOL(<stmnt>).
      DATA(statement) = VALUE ty_statement(
        terminator  = <stmnt>-terminator
        type        = <stmnt>-type
        tokens      = VALUE ty_tokens(
          FOR token IN tokens_raw FROM <stmnt>-from TO <stmnt>-to
          ( value             = token-str
            value_no_modifier = token-str
            row               = token-row
            col               = token-col
            type              = token-type ) ) ).

      statement-first_token = statement-tokens[ 1 ]-value.
      statement-is_main_query = xsdbool( statement-first_token = c_keywords-select OR
                                         statement-first_token = c_keywords-with ).

      stmnts = VALUE #( BASE stmnts ( statement ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD determine_main_stmnt_props.
    DATA: select_index TYPE sy-tabix.

    FIELD-SYMBOLS: <token> LIKE LINE OF tokens_raw.

    is_single_result = abap_false.

    " Determine the last select statement in the query
    IF line_exists( tokens_raw[ str = c_keywords-with ] ).
      select_index = lines( tokens_raw ).

      WHILE select_index > 0.
        IF tokens_raw[ select_index ]-str = c_keywords-select.
          " If it is the last main select, the preceding token has to be a parenthesis
          IF tokens_raw[ select_index - 1 ]-str = ')'.
            EXIT.
          ENDIF.
        ENDIF.
        select_index = select_index - 1.
      ENDWHILE.

    ELSE.
      select_index = line_index( tokens_raw[ str = c_keywords-select ] ).
    ENDIF.

    " Check if INTO TABLE is possible for the query
    LOOP AT tokens_raw ASSIGNING <token> FROM select_index WHERE str IN single_result_keywords.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      LOOP AT tokens_raw ASSIGNING <token> FROM select_index WHERE str IN aggregate_func_range.
        EXIT.
      ENDLOOP.
      is_single_result = xsdbool( sy-subrc <> 0 ).
    ENDIF.

    LOOP AT tokens_raw ASSIGNING <token> FROM select_index WHERE str = c_keywords-union.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      query_type = c_keywords-union.
    ELSE.
      query_type = c_keywords-select.
    ENDIF.

  ENDMETHOD.


  METHOD extract_parameters.
    LOOP AT stmnts ASSIGNING FIELD-SYMBOL(<stmnt>) WHERE first_token = c_keywords-data.

      DATA(parameter) = CAST ty_parameter( NEW lcl_query_param_parser( <stmnt>-tokens )->parse( ) ).
      IF parameter->name IS NOT INITIAL.
        parameters = VALUE #( BASE parameters ( parameter->* ) ).
      ENDIF.

      DELETE stmnts.
    ENDLOOP.
  ENDMETHOD.


  METHOD insert_into_table_clause.
*.. Find select/with statement
    ASSIGN stmnts[ is_main_query = abap_true ] TO FIELD-SYMBOL(<select_stmnt>).
    CHECK sy-subrc = 0.

    DATA(last_token) = <select_stmnt>-tokens[ lines( <select_stmnt>-tokens ) ].

*.. find position in query lines
    ASSIGN query_lines[ last_token-row ] TO FIELD-SYMBOL(<query_line>).

    select_query_end_offset = last_token-col + strlen( last_token-value ).
    select_query_end_row = last_token-row.
    IF is_single_result = abap_true.
      <query_line> = |{ <query_line>(select_query_end_offset) } INTO @DATA(result).|.
    ELSE.
      <query_line> = |{ <query_line>(select_query_end_offset) } INTO TABLE @DATA(result).|.
    ENDIF.
  ENDMETHOD.


  METHOD parse_query.

  ENDMETHOD.


  METHOD pre_validate_statements.
    DATA: select_stmnt_count TYPE i.

    FIELD-SYMBOLS: <token> TYPE stokesx.

    DATA(stmnt_count) = lines( stmnts_raw ).

    LOOP AT stmnts_raw ASSIGNING FIELD-SYMBOL(<stmnt>).
      DATA(index) = sy-tabix.

      ASSIGN tokens_raw[ <stmnt>-from ] TO FIELD-SYMBOL(<first_token>).

      IF <first_token>-str = c_keywords-select OR
         <first_token>-str = c_keywords-with.
        ADD 1 TO select_stmnt_count.
        select_stmnt_index = <first_token>-row.
      ELSEIF <first_token>-str = c_keywords-data.
        LOOP AT tokens_raw ASSIGNING <token> FROM <stmnt>-from TO <stmnt>-to WHERE str = 'LIKE'
                                                                                OR str = 'TABLE'.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error
            EXPORTING
              textid      = zcx_qdrt_sql_query_error=>invalid_token_in_data_declare
              msgv1       = |{ <token>-str }|
              line_number = <token>-row.
        ENDIF.

      ELSEIF <first_token>-str <> c_keywords-data AND
             <first_token>-str <> c_keywords-with AND
             <first_token>-str <> c_keywords-select.
        RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error
          EXPORTING
            textid      = zcx_qdrt_sql_query_error=>invalid_statement
            msgv1       = |{ <first_token>-str }|
            line_number = <first_token>-row.
      ENDIF.

      IF index = stmnt_count AND
         <first_token>-str <> c_keywords-select AND
         <first_token>-str <> c_keywords-with.
        RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error
          EXPORTING
            text        = |Last statement has to be a SELECT or WITH statment|
            line_number = <first_token>-row.
      ENDIF.
    ENDLOOP.

    IF select_stmnt_count > 1.
      RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error
        EXPORTING
          textid = zcx_qdrt_sql_query_error=>too_many_select_stmnt.
    ENDIF.

  ENDMETHOD.


  METHOD pre_validate_tokens.
    LOOP AT tokens_raw ASSIGNING FIELD-SYMBOL(<token>) WHERE str IN invalid_keyword_range.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error
        EXPORTING
          textid      = zcx_qdrt_sql_query_error=>invalid_token
          msgv1       = |{ <token>-str }|
          line_number = <token>-row.
    ENDIF.
  ENDMETHOD.


  METHOD simplify_tokens.
    select_stmnt-tokens = NEW lcl_query_token_simplifier( select_stmnt-tokens )->simplify( ).
  ENDMETHOD.


  METHOD tokenize.

    DATA: message     TYPE string,
          word        TYPE char80,
          line        TYPE i,
          db_entities TYPE TABLE OF tabname.

    SCAN ABAP-SOURCE query_lines TOKENS INTO     tokens_raw
                                 STATEMENTS INTO stmnts_raw
                                 MESSAGE INTO    message
                                 WORD INTO       word
                                 LINE INTO       line
                                 WITH ANALYSIS.

    IF message IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error
        EXPORTING
          textid = zcx_qdrt_appl_error=>general_error
          msgv1  = |{ message }|
          msgv2  = |{ word }|
          msgv3  = |{ line }|.
    ENDIF.

    LOOP AT stmnts_raw ASSIGNING FIELD-SYMBOL(<stmnt>).

      CALL FUNCTION 'RS_QUALIFY_ABAP_TOKENS_STR'
        EXPORTING
          statement_type = <stmnt>-type
          index_from     = <stmnt>-from
          index_to       = <stmnt>-to
        CHANGING
          stokesx_tab    = tokens_raw
        EXCEPTIONS
          OTHERS         = 0.

    ENDLOOP.

*.. extract db entities from query tokens
    LOOP AT tokens_raw ASSIGNING FIELD-SYMBOL(<token_data>) WHERE type = sana_tok_type
                                                              AND str IS NOT INITIAL.
      CHECK <token_data>-str(1) <> '+'.
      DATA(token) = <token_data>-str.
      REPLACE ALL OCCURRENCES OF '(' IN token WITH space.
      db_entities = VALUE #( BASE db_entities ( CONV #( token ) ) ).
    ENDLOOP.

    IF sy-subrc = 0.
      SORT db_entities.
      DELETE ADJACENT DUPLICATES FROM db_entities.
      query_entities = VALUE #( FOR entity IN db_entities ( sign = 'I' option = 'EQ' low = entity ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
