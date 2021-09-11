"! <p class="shorttext synchronized" lang="en">Constants for Range Options</p>
INTERFACE zif_qdrt_c_filter_ops
  PUBLIC .

  CONSTANTS:
    default              TYPE ddoption VALUE space ##NO_TEXT,
    including            TYPE ddsign VALUE 'I' ##NO_TEXT,
    excluding            TYPE ddsign VALUE 'E' ##NO_TEXT,
    "! <p class="shorttext synchronized" lang="en">Search Value lies between 2 values</p>
    between              TYPE ddoption VALUE 'BT' ##NO_TEXT,
    "! <p class="shorttext synchronized" lang="en">Search Value does not lie between two values</p>
    not_between          TYPE ddoption VALUE 'NB' ##NO_TEXT,
    equals               TYPE ddoption VALUE 'EQ' ##NO_TEXT,
    not_equals           TYPE ddoption VALUE 'NE' ##NO_TEXT,
    greater_than         TYPE ddoption VALUE 'GT' ##NO_TEXT,
    greater_equal        TYPE ddoption VALUE 'GE' ##NO_TEXT,
    lesser_than          TYPE ddoption VALUE 'LT' ##NO_TEXT,
    lesser_equal         TYPE ddoption VALUE 'LE' ##NO_TEXT,
    contains_pattern     TYPE ddoption VALUE 'CP' ##NO_TEXT,
    not_contains_pattern TYPE ddoption VALUE 'NP' ##NO_TEXT,
    contains_string      TYPE ddoption VALUE 'CS' ##NO_TEXT,
    not_constains_string TYPE ddoption VALUE 'NS' ##NO_TEXT,
    is_null              TYPE ddoption VALUE 'IN' ##no_text,
    is_not_null          TYPE ddoption VALUE 'NN' ##no_text,
    not_in_subquery      TYPE ddoption VALUE 'S1' ##NO_TEXT,
    in_subquery          TYPE ddoption VALUE 'S2' ##NO_TEXT,
    exists_subquery      TYPE ddoption VALUE 'S3' ##NO_TEXT,
    not_exists_subquery  TYPE ddoption VALUE 'S4' ##NO_TEXT.
ENDINTERFACE.
