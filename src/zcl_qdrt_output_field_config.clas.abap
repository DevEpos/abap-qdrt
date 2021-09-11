"! <p class="shorttext synchronized" lang="en">Output field configuration</p>
CLASS zcl_qdrt_output_field_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_output_field_config.

    METHODS:
      constructor
        IMPORTING
          output_fields TYPE zif_qdrt_output_field_config=>ty_output_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_select_all TYPE string VALUE '*'.

    DATA:
      output_fields         TYPE zif_qdrt_output_field_config=>ty_output_fields,
      use_count             TYPE abap_bool,
      select_only_specified TYPE abap_bool.
ENDCLASS.



CLASS zcl_qdrt_output_field_config IMPLEMENTATION.


  METHOD constructor.
    me->output_fields = output_fields.
  ENDMETHOD.


  METHOD zif_qdrt_output_field_config~get_select_clause.
    FIELD-SYMBOLS:
      <result_line> TYPE string.

    IF output_fields IS INITIAL AND select_only_specified = abap_false.
      result = VALUE #( ( c_select_all ) ).
    ELSE.

      LOOP AT output_fields ASSIGNING FIELD-SYMBOL(<output_field>).
        IF <result_line> IS ASSIGNED.
          <result_line> = |{ <result_line> }, |.
        ENDIF.

        APPEND INITIAL LINE TO result ASSIGNING <result_line>.
        IF <output_field>-function IS NOT INITIAL.
          <result_line> = |{ <output_field>-function }( { <output_field>-field_name } ) AS !{ <output_field>-field_name }|.
        ELSE.
          <result_line> = |{ <output_field>-field_name } AS !{ <output_field>-field_name }|.
        ENDIF.
      ENDLOOP.

      IF use_count = abap_true.
        IF <result_line> IS ASSIGNED.
          <result_line> = |{ <result_line> }, |.
        ENDIF.
        result = VALUE #( BASE result ( |COUNT( * ) AS { zif_qdrt_c_global=>c_custom_field_names-line_index }| ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_qdrt_output_field_config~set_use_count_field.
    use_count = value.
  ENDMETHOD.


  METHOD zif_qdrt_output_field_config~set_select_only_specified.
    select_only_specified = abap_true.
  ENDMETHOD.


  METHOD zif_qdrt_output_field_config~is_output_field.
    IF output_fields IS INITIAL AND select_only_specified = abap_false.
      result = abap_true.
    ELSE.
      result = xsdbool( line_exists( output_fields[ field_name = fieldname ] ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_qdrt_output_field_config~has_aggr_fields.

    LOOP AT output_fields ASSIGNING FIELD-SYMBOL(<output_field>) WHERE function IS NOT INITIAL.
      EXIT.
    ENDLOOP.

    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

ENDCLASS.
