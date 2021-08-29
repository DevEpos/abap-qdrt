"! <p class="shorttext synchronized" lang="en">Filter Provider</p>
INTERFACE zif_qdrt_filter_provider
  PUBLIC .

  TYPES:
    BEGIN OF ty_filter_item,
      key TYPE string,
    END OF ty_filter_item,
    ty_filter_items TYPE STANDARD TABLE OF ty_filter_item WITH EMPTY KEY,

    BEGIN OF ty_filter_range,
      key_field TYPE string,
      operation TYPE string,
      value1    TYPE string,
      value2    TYPE string,
      exclude   TYPE abap_bool,
    END OF ty_filter_range,
    ty_filter_ranges TYPE STANDARD TABLE OF ty_filter_range WITH EMPTY KEY,

    BEGIN OF ty_filter,
      field_name TYPE string,
      value      TYPE string,
      items      TYPE ty_filter_items,
      ranges     TYPE ty_filter_ranges,
    END OF ty_filter,

    ty_filters TYPE STANDARD TABLE OF ty_filter WITH EMPTY KEY.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Converts all filters to a where condition</p>
    get_filter_string
      RETURNING
        VALUE(result) TYPE string_table.
ENDINTERFACE.
