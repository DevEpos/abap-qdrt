"! <p class="shorttext synchronized" lang="en">Filter provider</p>
CLASS zcl_qdrt_entity_fp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_qdrt_filter_provider.

    METHODS:
      constructor
        IMPORTING
          field_filters TYPE zif_qdrt_filter_provider=>ty_filters.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_selopt.
             INCLUDE TYPE zif_qdrt_ty_global=>ty_selopt.
    TYPES:   subquery TYPE string.
    TYPES: END OF ty_selopt.
    TYPES: ty_selopts TYPE STANDARD TABLE OF ty_selopt WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_field_selopt,
        sqlfieldname TYPE zif_qdrt_ty_global=>ty_sql_fieldname,
        field        TYPE zif_qdrt_ty_global=>ty_fieldname_with_alias,
        sql_function TYPE zif_qdrt_ty_global=>ty_sql_function,
        options      TYPE ty_selopts,
      END OF ty_field_selopt,
      ty_field_selopts TYPE STANDARD TABLE OF ty_field_selopt WITH EMPTY KEY.

    DATA: field_filters TYPE zif_qdrt_filter_provider=>ty_filters.

    METHODS:
      get_option
        IMPORTING
          original_sign   TYPE ddsign
          original_option TYPE ddoption
          high            TYPE clike
          escape_char     TYPE abap_bool OPTIONAL
        CHANGING
          option          TYPE ddoption
          low             TYPE clike,
      free_selections_to_where
        IMPORTING
          field_range   TYPE ty_field_selopts
        RETURNING
          VALUE(result) TYPE string_table,
      create_where
        IMPORTING
          seltab_sql    TYPE zif_qdrt_ty_global=>ty_selopts_sql
        RETURNING
          VALUE(result) TYPE string_table ,
      add_field_conditions
        IMPORTING
          field_sel    TYPE ty_field_selopt
          where_clause TYPE REF TO zcl_qdrt_sql_where_clause,
      add_fieldname_to_cond
        IMPORTING
          fieldname    TYPE zif_qdrt_ty_global=>ty_sql_fieldname
          sql_function TYPE zif_qdrt_ty_global=>ty_sql_function OPTIONAL
          where_clause TYPE REF TO zcl_qdrt_sql_where_clause
        CHANGING
          low_val      TYPE zif_qdrt_ty_global=>ty_filter_value OPTIONAL
          high_val     TYPE zif_qdrt_ty_global=>ty_filter_value OPTIONAL,
      add_std_condition
        IMPORTING
          fieldname    TYPE zif_qdrt_ty_global=>ty_sql_fieldname
          sql_function TYPE zif_qdrt_ty_global=>ty_sql_function OPTIONAL
          selopt       TYPE ty_selopt
          where_clause TYPE REF TO zcl_qdrt_sql_where_clause,
      add_subquery_condition
        IMPORTING
          fieldname    TYPE zif_qdrt_ty_global=>ty_sql_fieldname
          subquery     TYPE string
          option       TYPE ddoption
          where_clause TYPE REF TO zcl_qdrt_sql_where_clause.
ENDCLASS.



CLASS zcl_qdrt_entity_fp IMPLEMENTATION.


  METHOD constructor.
    me->field_filters = field_filters.
  ENDMETHOD.


  METHOD zif_qdrt_filter_provider~get_filter_string.
    DATA: seltab TYPE zif_qdrt_ty_global=>ty_selopts_sql.

    LOOP AT field_filters ASSIGNING FIELD-SYMBOL(<field_filter>).

      LOOP AT <field_filter>-items ASSIGNING FIELD-SYMBOL(<filter_item>).
        seltab = VALUE #( BASE seltab
          ( field  = <field_filter>-field_name
            sign   = zif_qdrt_c_filter_ops=>including
            option = zif_qdrt_c_filter_ops=>equals
            low    = <filter_item>-key ) ).
      ENDLOOP.

      LOOP AT <field_filter>-ranges ASSIGNING FIELD-SYMBOL(<filter_range>).
        seltab = VALUE #( BASE seltab
          ( field  = <field_filter>-field_name
            sign   = COND #( WHEN <filter_range>-exclude = abap_true THEN zif_qdrt_c_filter_ops=>excluding
                             ELSE zif_qdrt_c_filter_ops=>including )
            option = zif_qdrt_c_filter_ops=>equals
            low    = <filter_item>-key ) ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.



  METHOD create_where.
    DATA: field_ranges TYPE ty_field_selopts,
          field_range  TYPE ty_field_selopt,
          selopt       TYPE ty_selopt,
          old_field    TYPE zif_qdrt_ty_global=>ty_sql_fieldname.

    CHECK: seltab_sql IS NOT INITIAL.

    LOOP AT seltab_sql ASSIGNING FIELD-SYMBOL(<selfield>).
      " Handle incomplete options
      DATA(fieldname) = COND #( WHEN <selfield>-sqlfieldname IS NOT INITIAL THEN <selfield>-sqlfieldname ELSE <selfield>-field ).
      CHECK fieldname IS NOT INITIAL.

      " New fieldname
      IF fieldname <> old_field AND
         old_field <> space.
        field_ranges = VALUE #( BASE field_ranges ( field_range ) ).
        CLEAR: field_range.
      ENDIF.

      field_range-field = <selfield>-field.
      field_range-sqlfieldname = <selfield>-sqlfieldname.
      field_range-sql_function = <selfield>-sql_function.

      IF field_range-sqlfieldname IS INITIAL.
        field_range-sqlfieldname = field_range-field.
      ENDIF.

      selopt = VALUE #(
        sign     = COND #( WHEN <selfield>-sign = space THEN zif_qdrt_c_filter_ops=>including ELSE <selfield>-sign )
        low      = <selfield>-low
        high     = <selfield>-high
        option   = <selfield>-option
        subquery = <selfield>-subquery ).

      get_option(
        EXPORTING
          original_sign   = selopt-sign
          original_option = <selfield>-option
          high            = <selfield>-high
        CHANGING
          option          = selopt-option
          low             = selopt-low ).

      field_range-options = VALUE #( BASE field_range-options ( selopt ) ).
      old_field = fieldname.
    ENDLOOP.

    " Complete last field values
    IF field_range-sqlfieldname IS NOT INITIAL.
      field_ranges = VALUE #( BASE field_ranges ( field_range ) ).
    ENDIF.

    CHECK field_ranges IS NOT INITIAL.

    result = free_selections_to_where( field_range = field_ranges ).
  ENDMETHOD.


  METHOD get_option.

    DATA: is_low_cp TYPE abap_bool.

    DATA(l_sign) = COND #( WHEN original_sign = space THEN 'I' ELSE original_sign ).

    IF ( low CS '*' OR low CS '+') AND
       ( escape_char <> abap_true ).
      is_low_cp = abap_true.
    ENDIF.


    IF original_option = space.
      IF high <> space AND
         low <> high.
        option = zif_qdrt_c_filter_ops=>between.
      ENDIF.
      IF is_low_cp = abap_true AND high IS INITIAL.
        option = zif_qdrt_c_filter_ops=>contains_pattern.
      ENDIF.
      IF option = space.
        option = zif_qdrt_c_filter_ops=>equals.
      ENDIF.
    ELSE.
      option = original_option.
    ENDIF.

    IF option = space.
      option = zif_qdrt_c_filter_ops=>equals.
    ENDIF.

  ENDMETHOD.

  METHOD free_selections_to_where.

    DATA(where_clause) = NEW zcl_qdrt_sql_where_clause( ).

    LOOP AT field_range ASSIGNING FIELD-SYMBOL(<ls_field_selection>).

      IF sy-tabix > 1.
        where_clause->start_new_line( 'AND' ).
      ENDIF.

      add_field_conditions(
        field_sel    = <ls_field_selection>
        where_clause = where_clause ).

    ENDLOOP.

    result = where_clause->get_result( ).

  ENDMETHOD.

  METHOD add_field_conditions.

    DATA: fieldname_length       TYPE i,
          including_filter_count TYPE i.

    FIELD-SYMBOLS: <ls_option> TYPE ty_selopt.

    " always use length of name to spare some spaces
    fieldname_length = strlen( field_sel-sqlfieldname ).
    IF NOT fieldname_length > 0.
      fieldname_length = 1.
    ENDIF.

    TRY.
        cl_abap_dyn_prg=>check_column_name( field_sel-field ).
      CATCH cx_abap_invalid_name.
        MESSAGE e080(sldbv) WITH field_sel-field.
    ENDTRY.

    IF field_sel-options IS INITIAL.
      RETURN.
    ENDIF.

    DATA(first_iteration) = abap_true.

    LOOP AT field_sel-options ASSIGNING <ls_option> WHERE sign = 'I'.

      IF first_iteration = abap_true.
        where_clause->add_word( '(' ).
        CLEAR first_iteration.
      ELSE.
        where_clause->add_word( 'OR' ).
      ENDIF.

      add_std_condition(
        fieldname        = field_sel-sqlfieldname
        sql_function     = field_sel-sql_function
        selopt           = <ls_option>
        where_clause     = where_clause ).
      ADD 1 TO including_filter_count.
    ENDLOOP.

    IF including_filter_count > 0.
      where_clause->add_word( ')' ).
    ENDIF.

    first_iteration = abap_true.

    LOOP AT field_sel-options ASSIGNING <ls_option> WHERE sign = 'E'.
      IF first_iteration = abap_false OR including_filter_count > 0.
        where_clause->add_word( 'AND' ).
      ENDIF.
      CLEAR first_iteration.
      add_std_condition(
        fieldname        = field_sel-sqlfieldname
        sql_function     = field_sel-sql_function
        selopt           = <ls_option>
        where_clause     = where_clause ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_fieldname_to_cond.
    DATA(l_fieldname) = fieldname.

    CASE sql_function.

      WHEN zif_qdrt_c_sql_function=>upper.
        IF sy-saprl >= 751.
          l_fieldname = |UPPER( { fieldname } )|.

          " Convert low/high to upper case
          IF low_val IS NOT INITIAL.
            TRANSLATE low_val TO UPPER CASE.
          ENDIF.

          IF high_val IS NOT INITIAL.
            TRANSLATE high_val TO UPPER CASE.
          ENDIF.
        ENDIF.

    ENDCASE.

    where_clause->add_word( CONV #( l_fieldname ) ).
  ENDMETHOD.

  METHOD add_subquery_condition.
    DATA: subquery_lines TYPE string_table.

    " Always start a new row for a subquery clause
    where_clause->start_new_line( ).

    IF option = zif_qdrt_c_filter_ops=>not_in_subquery OR
        option = zif_qdrt_c_filter_ops=>in_subquery.

      add_fieldname_to_cond(
        fieldname    = fieldname
        where_clause = where_clause ).

      IF option = zif_qdrt_c_filter_ops=>not_in_subquery.
        where_clause->add_word( 'NOT' ).
      ENDIF.

      where_clause->add_word( 'IN' ).
      " Wrap subquery in parenthesis
      where_clause->add_word( '(' ).

      " Add subquery clause
      SPLIT subquery AT cl_abap_char_utilities=>cr_lf INTO TABLE subquery_lines.
      LOOP AT subquery_lines ASSIGNING FIELD-SYMBOL(<lv_query_line>).
        where_clause->start_new_line( )->add_word(
          word        = <lv_query_line>
          word_length = strlen( <lv_query_line> ) ).
      ENDLOOP.
      " Close the subquery clause
      where_clause->add_word( ')' ).
    ELSE.
      " Exists is not supported yet
    ENDIF.
  ENDMETHOD.

  METHOD add_std_condition.

    DATA: low_val          TYPE zif_qdrt_ty_global=>ty_filter_value,
          high_val         TYPE zif_qdrt_ty_global=>ty_filter_value,
          option           TYPE ddoption,
          sql_pattern      TYPE string,
          is_escape_needed TYPE abap_bool.

    DATA: BEGIN OF escape_char,
            quote_1 TYPE c LENGTH 1 VALUE '''',
            escape,
            quote_2 TYPE c LENGTH 1 VALUE '''',
          END   OF escape_char.

    " Handle some special options
    CASE selopt-option.

      WHEN zif_qdrt_c_filter_ops=>not_in_subquery OR
           zif_qdrt_c_filter_ops=>in_subquery OR
           zif_qdrt_c_filter_ops=>not_exists_subquery OR
           zif_qdrt_c_filter_ops=>exists_subquery.

        add_subquery_condition(
          fieldname    = fieldname
          subquery     = selopt-subquery
          option       = selopt-option
          where_clause = where_clause ).
        RETURN.
    ENDCASE.

    low_val = |'{ replace( val = condense( selopt-low ) sub = `'` with = `''` occ = 0 ) }'|.

    IF selopt-option = zif_qdrt_c_filter_ops=>between OR
       selopt-option = zif_qdrt_c_filter_ops=>not_between.
      high_val = |'{ replace( val = condense( selopt-high ) sub = `'` with = `''` occ = 0 ) }'|.
    ENDIF.

    MOVE selopt-option TO option.

    IF selopt-sign = zif_qdrt_c_filter_ops=>excluding.
      CASE option.
        WHEN zif_qdrt_c_filter_ops=>equals.
          option = zif_qdrt_c_filter_ops=>not_equals.
        WHEN zif_qdrt_c_filter_ops=>not_equals.
          option = zif_qdrt_c_filter_ops=>equals.
        WHEN zif_qdrt_c_filter_ops=>between.
          option = zif_qdrt_c_filter_ops=>not_between.
        WHEN zif_qdrt_c_filter_ops=>lesser_equal.
          option = '> '.
        WHEN zif_qdrt_c_filter_ops=>greater_equal.
          option = '< '.
        WHEN zif_qdrt_c_filter_ops=>lesser_than.
          option = zif_qdrt_c_filter_ops=>greater_equal.
        WHEN zif_qdrt_c_filter_ops=>greater_than.
          option = zif_qdrt_c_filter_ops=>lesser_equal.
        WHEN zif_qdrt_c_filter_ops=>contains_pattern.
          option = zif_qdrt_c_filter_ops=>not_contains_pattern.
        WHEN zif_qdrt_c_filter_ops=>not_contains_pattern.
          option = zif_qdrt_c_filter_ops=>contains_pattern.
        WHEN zif_qdrt_c_filter_ops=>not_between.
          option = zif_qdrt_c_filter_ops=>between.
      ENDCASE.
    ELSE.
      CASE option.
        WHEN zif_qdrt_c_filter_ops=>lesser_than.
          option = '< '.
        WHEN zif_qdrt_c_filter_ops=>greater_than.
          option = '> '.
      ENDCASE.
    ENDIF.

    add_fieldname_to_cond(
      EXPORTING fieldname    = fieldname
                sql_function = sql_function
                where_clause = where_clause
      CHANGING  low_val      = low_val
                high_val     = high_val ).


*.. TODO: If Between is active comparator and sql function is supplied
*.......... it has to be converted to the following pattern:
*.......... => [not (] function( field) >= value and function( field ) <= value [)]

    CASE option.

        " Between Option -> low and high have to be filled
      WHEN zif_qdrt_c_filter_ops=>between OR
           zif_qdrt_c_filter_ops=>not_between.

        " Negate the option
        IF option = zif_qdrt_c_filter_ops=>not_between.
          where_clause->add_word( 'NOT' ).
        ENDIF.

        where_clause->add_word( 'BETWEEN' ).
        where_clause->add_word( CONV #( low_val ) ).
        where_clause->add_word( 'AND' ).
        where_clause->add_word( CONV #( high_val ) ).

        " Compare Pattern option i.e. '*word*'
      WHEN zif_qdrt_c_filter_ops=>contains_pattern OR
           zif_qdrt_c_filter_ops=>not_contains_pattern.

        " Negate the option
        IF option = zif_qdrt_c_filter_ops=>not_contains_pattern.
          where_clause->add_word( 'NOT' ).
        ENDIF.

        where_clause->add_word( 'LIKE' ).

        zcl_qdrt_wildcard_pattern_conv=>conv_sap_to_sql_pattern(
          EXPORTING
            sap_pattern    = low_val
          IMPORTING
            sql_pattern    = sql_pattern
            escape_needed  = is_escape_needed ).

        low_val = sql_pattern.

        IF is_escape_needed = abap_true.
          escape_char-escape = '#'.
        ENDIF.

        where_clause->add_word( CONV #( low_val ) ).

        IF escape_char-escape <> space.
          where_clause->add_word( 'ESCAPE' ).
          where_clause->add_word( CONV #( escape_char ) ).
        ENDIF.
        " Handle IS NULL/ NOT IS NULL
      WHEN zif_qdrt_c_filter_ops=>is_not_null OR
           zif_qdrt_c_filter_ops=>is_null.

        IF option = zif_qdrt_c_filter_ops=>is_not_null.
          where_clause->add_word( 'IS NOT NULL' ).
        ELSE.
          where_clause->add_word( 'IS NULL' ).
        ENDIF.
        " The rest of the option can be handled in a simple manner
      WHEN OTHERS.
        where_clause->add_word( CONV #( option ) ).
        where_clause->add_word( CONV #( low_val ) ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
