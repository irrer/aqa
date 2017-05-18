
-- Potentially useful database queries

-----------------------------------------------------------------------------

-- create the equivalent column of the LOC 'Mean Across Sections column :
select "leafIndex", avg("transmission_fract") from "leafTransmission" where "outputPK" = 63 group by "leafIndex" order by "leafIndex";

-----------------------------------------------------------------------------

-- create the equivalent row of the LOC 'Mean' for the 5 sections.
-- Change 'avg' to 'stddev' to get the STD row.
select "section", avg("transmission_fract") from "leafTransmission" where "outputPK" = 63 group by "section" order by "section";

-----------------------------------------------------------------------------

-- create the equivalent row of the LOC 'Coeff. of Var.' for the 5 sections.
select "section", stddev("transmission_fract") / avg("transmission_fract") from "leafTransmission" where "outputPK" = 63 group by "section" order by "section";

with all_sect as (
    select "transmission_fract" as s1
    from "leafTransmission" 
    group by "section", "leafIndex"
    order by "section"
)
select * from all_sect;

-----------------------------------------------------------------------------

-- select the transmission values and display them in a table

with

opk as ( select 108 as val),

sect1 as (
    select "leafIndex", "transmission_fract"
    from "leafTransmission", opk where "outputPK" = opk.val and "section" = '1'
    group by "leafIndex", "transmission_fract"
),

sect2 as (
    select "leafIndex", "transmission_fract"
    from "leafTransmission", opk where "outputPK" = opk.val and "section" = '2'
    group by "leafIndex", "transmission_fract"
),

sect3 as (
    select "leafIndex", "transmission_fract"
    from "leafTransmission", opk where "outputPK" = opk.val and "section" = '3'
    group by "leafIndex", "transmission_fract"
),

sect4 as (
    select "leafIndex", "transmission_fract"
    from "leafTransmission", opk where "outputPK" = opk.val and "section" = '4'
    group by "leafIndex", "transmission_fract"
),

sect5 as (
    select "leafIndex", "transmission_fract"
    from "leafTransmission", opk where "outputPK" = opk.val and "section" = '5'
    group by "leafIndex", "transmission_fract"
)

select
    "sect1"."leafIndex",
    "sect1"."transmission_fract",
    "sect2"."transmission_fract",
    "sect3"."transmission_fract",
    "sect4"."transmission_fract",
    "sect5"."transmission_fract"
from
    sect1, sect2, sect3, sect4, sect5
where
    sect1."leafIndex" = sect2."leafIndex" and
    sect1."leafIndex" = sect3."leafIndex" and
    sect1."leafIndex" = sect4."leafIndex" and
    sect1."leafIndex" = sect5."leafIndex"
    order by "leafIndex"
;
