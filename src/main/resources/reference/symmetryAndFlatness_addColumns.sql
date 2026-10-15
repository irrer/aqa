
-- Column: public."symmetryAndFlatness"."KVP"

-- ALTER TABLE IF EXISTS public."symmetryAndFlatness" DROP COLUMN IF EXISTS "KVP";

ALTER TABLE IF EXISTS public."symmetryAndFlatness"
    ADD COLUMN "KVP" double precision;

---------------------------------------------------------------------------------------------------

-- Column: public."symmetryAndFlatness"."isFlatteningFilterFree"

-- ALTER TABLE IF EXISTS public."symmetryAndFlatness" DROP COLUMN IF EXISTS "isFlatteningFilterFree";

ALTER TABLE IF EXISTS public."symmetryAndFlatness"
    ADD COLUMN "isFlatteningFilterFree" boolean;

