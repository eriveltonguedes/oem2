### scripts gerais

SELECT ano, mes, processo, anodoprocesso, tipo_pf_pj, cpf_cnpj, substancia, uf, municipio, quantidadecomercializada, unidadedemedida, valorrecolhido, codigo7
FROM public.tb_cfem_arrecadacao;


UPDATE public.tb_cfem_arrecadacao
SET codigo7=0
where codigo7 is null;


SELECT codigo7, ano, mes, processo, anodoprocesso, tipo_pf_pj, cpf_cnpj, substancia, uf, municipio, quantidadecomercializada, unidadedemedida, valorrecolhido, codigo7
FROM public.tb_cfem_arrecadacao
where codigo7 =0;


SELECT count(*), sum(valorrecolhido) FROM public.tb_cfem_arrecadacao where codigo7 =0;  --14.380  // 9306 

SELECT count(*), sum(valorrecolhido) FROM public.tb_cfem_arrecadacao ; -- 1.612.232	36.231.565.312
                                                                       --     9.306	    33.893.100
																		--    1.655	     6.848.270


SELECT valorrecolhido::int4 as vlr, valorrecolhido , ano, mes, processo, anodoprocesso, tipo_pf_pj, cpf_cnpj, substancia, uf, municipio, quantidadecomercializada, unidadedemedida,  codigo7
FROM public.tb_cfem_arrecadacao
where codigo7 =0
order by valorrecolhido desc;

 
UPDATE public.tb_cfem_arrecadacao
SET codigo7=subquery.edterritorios_codigo
FROM (SELECT uf, municipio, edterritorios_codigo
FROM public.tb_municipios_de_para
where  edterritorios_codigo is not null) AS subquery
where codigo7 =0 and tb_cfem_arrecadacao.uf=subquery.uf and tb_cfem_arrecadacao.municipio = subquery.municipio

@set param1='31'
@set param2='YYY'

select x,y from tabA where col1=:param1
UNION
select t,u from tabB where col5=:param1 and col6=:param2


@set uuf = '24'
@set mun='O'
SELECT edterritorios_codigo, edterritorios_nome, municipio_rfb ,edterritorios_sigla, edterritorios_geometry, edterritorios_centroide, edterritorios_bounding_box, edterritorios_tnivid, edterritorios_data_inicial, edterritorios_data_final, edterritorios_id, edterritorios_terid, edterritorios_pk, edterritorios_terarea, geog_centroide, edterritorios_geography, lixo_teste, edterritorios_codigo6, edterritorios_sigla_uf, codemun_rfb, municipio_rfb, mudou_nome, edterritorios_ano_instalacao, edterritorios_altitude_m
FROM spat.ed_territorios_municipios
where edterritorios_sigla =:uuf  and edterritorios_nome > :mun
order by edterritorios_nome 


SELECT uf, municipio, edterritorios_codigo, qtd, valor
FROM public.tb_municipios_de_para where edterritorios_codigo is null
order by uf, municipio ;


SELECT edterritorios_codigo, edterritorios_nome, municipio_rfb ,edterritorios_sigla, edterritorios_geometry, edterritorios_centroide, edterritorios_bounding_box, edterritorios_tnivid, edterritorios_data_inicial, edterritorios_data_final, edterritorios_id, edterritorios_terid, edterritorios_pk, edterritorios_terarea, geog_centroide, edterritorios_geography, lixo_teste, edterritorios_codigo6, edterritorios_sigla_uf, codemun_rfb, municipio_rfb, mudou_nome, edterritorios_ano_instalacao, edterritorios_altitude_m
FROM spat.ed_territorios_municipios where edterritorios_codigo = 2401305

TO	S?O VAL?RIO