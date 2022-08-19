SELECT numerodedistribuicao, ano, mes, ente, siglaestado, nomeente, tipodistribuicao, substancia, tipoafetamento, valor, codigo7 
FROM public.tb_cfem_distribuicao where ente <> 'UF';

--

--passo 1: codigos encontrados  
UPDATE public.tb_cfem_distribuicao
SET  codigo7=foo.edterritorios_codigo
from (SELECT edterritorios_codigo, upper(edterritorios_sigla_uf) as uf2, upper(edterritorios_nome::text) as nome2  FROM  spat.ed_territorios_municipios) as foo
where codigo7 is null and upper(siglaestado::bpchar) = foo.uf2 AND upper(nomeente::text) = foo.nome2;
  

--em seguida, buscando nome RFB
UPDATE public.tb_cfem_distribuicao
SET  codigo7=foo.edterritorios_codigo
from (SELECT edterritorios_codigo, upper(edterritorios_sigla_uf) as uf2, upper(municipio_rfb::text) as nome2  FROM  spat.ed_territorios_municipios) as foo
where codigo7 is null and upper(siglaestado::bpchar) = foo.uf2 AND upper(nomeente::text) = foo.nome2;
  

--em seguida, buscando nome no de-para

UPDATE public.tb_cfem_distribuicao
SET codigo7=subquery.edterritorios_codigo
FROM (SELECT uf, municipio, edterritorios_codigo
FROM public.tb_municipios_de_para
where  edterritorios_codigo is not null) AS subquery
where (codigo7=0  or codigo7 is null) and tb_cfem_distribuicao.siglaestado=subquery.uf and tb_cfem_distribuicao.nomeente = subquery.municipio;


SELECT siglaestado, nomeente, count(*) as qt, sum(valor)::int as valor 
FROM public.tb_cfem_distribuicao where ente <> 'UF' and codigo7 is null
group by siglaestado, nomeente;


SELECT count(*) FROM public.tb_cfem_distribuicao;  --687828

--comparando PA
select ano, nomeente, round(avg(valor),1) FROM public.tb_cfem_distribuicao
where siglaestado = 'PA' and ano=2020
group by ano, nomeente ;  

to_char(10, '99.99');

SELECT uf, municipio, edterritorios_codigo, qtd, valor
FROM public.tb_municipios_de_para where uf = 'PA';
