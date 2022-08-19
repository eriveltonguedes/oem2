-- public.vw_cfem_arrecadacao_vlr_ano_uf source

CREATE OR REPLACE VIEW public.vw_cfem_arrecadacao_vlr_ano_uf_s78
AS SELECT 
    78 as serie
    ,3 as abrangencia
    ,"substring"(m.edterritorios_codigo::text, 1, 2) as cod
    ,' ' as nome
    ,sum(c.valorrecolhido) AS valor
    ,(c.ano::text || '-01-01'::text)::date AS periodo
   FROM tb_cfem_arrecadacao c
     LEFT JOIN spat.ed_territorios_municipios m ON c.uf::bpchar = m.edterritorios_sigla_uf AND upper(c.municipio::text) = upper(m.edterritorios_nome::text)
  GROUP BY c.ano, ("substring"(m.edterritorios_codigo::text, 1, 2));

SELECT serie, abrangencia, cod, nome, valor, periodo
FROM public.vw_cfem_arrecadacao_vlr_ano_uf_s78 limit 5;


 /*
serie;abrangencia;cod;nome;valor;periodo
1;3;RJ;Rio de Janeiro;21;2018
1;3;SP;São Paulo;11;2018
2;3;RJ;Rio de Janeiro;12;2018
2;3;SP;São Paulo;25;2018

Abrangências
1 - País
2 - Região
3 - UF
4 - Municípios
 */