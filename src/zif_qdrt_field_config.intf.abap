"! <p class="shorttext synchronized" lang="en">Field configuration</p>
INTERFACE zif_qdrt_field_config
  PUBLIC.

  DATA:
    name               TYPE string READ-ONLY,
    is_key             TYPE abap_bool READ-ONLY,
    rollname           TYPE rollname READ-ONLY,
    type               TYPE string READ-ONLY,
    is_numeric         TYPE abap_bool READ-ONLY,
    is_total_possible  TYPE abap_bool READ-ONLY,
    max_length         TYPE i READ-ONLY,
    precision          TYPE i READ-ONLY,
    scale              TYPE i READ-ONLY,
    short_description  TYPE string READ-ONLY,
    medium_description TYPE string READ-ONLY,
    long_description   TYPE string READ-ONLY,
    fieldtext          TYPE string READ-ONLY,
    semantics          TYPE string READ-ONLY,
    unit_field         TYPE string READ-ONLY,
    has_value_help     TYPE abap_bool READ-ONLY,
    display_format     TYPE string READ-ONLY,
    value_help_type    TYPE string READ-ONLY.

ENDINTERFACE.
