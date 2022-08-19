SELECT ano, mes, processo, anodoprocesso, tipo_pf_pj, cpf_cnpj, substancia, uf, municipio, quantidadecomercializada, unidadedemedida, valorrecolhido, codigo7
,edterritorios_geography
FROM public.tb_cfem_arrecadacao c left join spat.ed_territorios_municipios m 
on c.codigo7 = m.edterritorios_codigo 
where ano=2021 and mes = 1;


SELECT ano, mes, processo, anodoprocesso, tipo_pf_pj, cpf_cnpj, substancia, uf, municipio, quantidadecomercializada, unidadedemedida, valorrecolhido, codigo7
,edterritorios_geography
FROM public.tb_cfem_arrecadacao c left join spat.ed_territorios_municipios m 
on c.codigo7 = m.edterritorios_codigo 
where ano=2021 and mes = 1;


-- public.vw_cfem_arrecadacao_vlr_ano source

CREATE OR REPLACE VIEW public.vw_cfem_arrecadacao_2021
AS 
SELECT ano,
    codigo7 ,
    sum(valorrecolhido) AS valor
   FROM tb_cfem_arrecadacao where ano=2021
  GROUP BY ano,
    codigo7;
    
  -- drop VIEW public.vw_cfem_arrecadacao_2021_geo;
  
   CREATE   VIEW public.vw_cfem_arrecadacao_2021_geo as  
   select ano,
    codigo7 ,
    valor::int2, edterritorios_geometry
from vw_cfem_arrecadacao_2021 c left join spat.ed_territorios_municipios m 
on c.codigo7 = m.edterritorios_codigo 


SELECT Populate_Geometry_Columns('spat.ed_territorios_municipios'::regclass);

SELECT Populate_Geometry_Columns('public.vw_cfem_arrecadacao_2021_geo'::regclass);

SELECT UpdateGeometrySRID('public.vw_cfem_arrecadacao_2021_geo','edterritorios_geometry',4674);

SELECT f_table_name, f_geometry_column, srid, type
	FROM geometry_columns
	where srid = 4326
	--WHERE f_table_name = 'vw_pois_ny_parks';

UPDATE edterritorios_geography
SET srid = 4674
WHERE f_table_name = vw_cfem_arrecadacao_2021_geo;