
-- Add the column: public.vmat."doseMLCPerOpen" for Postgresql
-- Sun Jun 21 2026

-- ALTER TABLE IF EXISTS public.vmat DROP COLUMN IF EXISTS "doseMLCPerOpen";

ALTER TABLE IF EXISTS public.vmat
    ADD COLUMN "doseMLCPerOpen" double precision;
