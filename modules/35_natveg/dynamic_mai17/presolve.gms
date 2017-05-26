*** (C) 2008-2017 Potsdam Institute for Climate Impact Research (PIK),
*** authors, and contributors see AUTHORS file
*** This file is part of MAgPIE and licensed under GNU AGPL Version 3
*** or later. See LICENSE file or go to http://www.gnu.org/licenses/
*** Contact: magpie@pik-potsdam.de

*shift age-classes according to time step length
s35_shift = m_yeardiff(t)/5;
if((ord(t) = 1),
	p35_secdforest(t,j,ac,"before") = i35_secdforest(j,ac);
	p35_other(t,j,ac,"before") = i35_other(j,ac);
else
	p35_other(t,j,ac,"before") =
				    0$(ord(ac) = 1)
                  + p35_other(t-1,j,ac-s35_shift,"after")$(ord(ac) > 1 AND ord(ac) < card(ac))
                  + p35_other(t-1,j,ac,"after")$(ord(ac) = card(ac));
	p35_secdforest(t,j,ac,"before") =
				    0$(ord(ac) = 1)
                  + p35_secdforest(t-1,j,ac-s35_shift,"after")$(ord(ac) > 1 AND ord(ac) < card(ac))
                  + p35_secdforest(t-1,j,ac,"after")$(ord(ac) = card(ac));
);


*calculate new v35_secdforest.l
pc35_secdforest(j,land35) = sum(ac_land35(ac,land35), p35_secdforest(t,j,ac,"before"));
v35_secdforest.l(j,land35) = pc35_secdforest(j,land35);
pcm_land(j,"secdforest") = sum(land35, pc35_secdforest(j,land35));
vm_land.l(j,"secdforest") = pcm_land(j,"secdforest");

*calculate new v35_other.l
pc35_other(j,land35) = sum(ac_land35(ac,land35), p35_other(t,j,ac,"before"));
v35_other.l(j,land35) = pc35_other(j,land35);
pcm_land(j,"other") = sum(land35, pc35_other(j,land35));
vm_land.l(j,"other") = pcm_land(j,"other");

*** Forest protection (WDPA areas and different conservation priority areas)
* calc protection share for primforest, secdforest and other land
pc35_natveg_old(j) = pcm_land(j,"primforest") + pc35_secdforest(j,"old") + pc35_other(j,"old");
p35_protect_shr(t,j,prot_type)$(pc35_natveg_old(j) > 0) = f35_protect_area(j,prot_type)/pc35_natveg_old(j);
p35_protect_shr(t,j,prot_type)$(p35_protect_shr(t,j,prot_type) > 1) = 1;
p35_protect_shr(t,j,prot_type)$(p35_protect_shr(t,j,prot_type) < 0) = 0;

* protection scenarios
$ifthen "%c35_protect_scenario%" == "none"
  p35_save_primforest(t,j) = 0;
  p35_save_secdforest(t,j) = 0;
  p35_save_other(t,j) = 0;
$elseif "%c35_protect_scenario%" == "full"
  p35_save_primforest(t,j) = pcm_land(j,"primforest");
  p35_save_secdforest(t,j) = pcm_land(j,"secdforest");
  p35_save_other(t,j) = pcm_land(j,"other");
$elseif "%c35_protect_scenario%" == "WDPA"
  p35_save_primforest(t,j) = p35_protect_shr(t,j,"WDPA")*pcm_land(j,"primforest");
  p35_save_secdforest(t,j) = p35_protect_shr(t,j,"WDPA")*pc35_secdforest(j,"old");
  p35_save_other(t,j) = p35_protect_shr(t,j,"WDPA")*pc35_other(j,"old");
$else
* conservation priority scenarios start in 2020, in addition to WDPA protection
  if ((sameas(t,"y1995") or sameas(t,"y2000") or sameas(t,"y2005") or sameas(t,"y2010") or sameas(t,"y2015")),
  p35_save_primforest(t,j) = p35_protect_shr(t,j,"WDPA")*pcm_land(j,"primforest");
  p35_save_secdforest(t,j) = p35_protect_shr(t,j,"WDPA")*pc35_secdforest(j,"old");
  p35_save_other(t,j) = p35_protect_shr(t,j,"WDPA")*pc35_other(j,"old");
  else
  p35_save_primforest(t,j) = (p35_protect_shr(t,j,"WDPA")+p35_protect_shr(t,j,"%c35_protect_scenario%"))*pcm_land(j,"primforest");
  p35_save_secdforest(t,j) = (p35_protect_shr(t,j,"WDPA")+p35_protect_shr(t,j,"%c35_protect_scenario%"))*pc35_secdforest(j,"old");
  p35_save_other(t,j) = (p35_protect_shr(t,j,"WDPA")+p35_protect_shr(t,j,"%c35_protect_scenario%"))*pc35_other(j,"old");
  p35_save_primforest(t,j)$(p35_save_primforest(t,j) > pcm_land(j,"primforest")) = pcm_land(j,"primforest");
  p35_save_secdforest(t,j)$(p35_save_secdforest(t,j) > pc35_secdforest(j,"old")) = pc35_secdforest(j,"old");
  p35_save_other(t,j)$(p35_save_other(t,j) > pc35_other(j,"old")) = pc35_other(j,"old");
  );
$endif

*set boundaries primforest
vm_land.lo(j,"primforest") = p35_save_primforest(t,j);
vm_land.up(j,"primforest") = pcm_land(j,"primforest");
m_boundfix(vm_land,(j,"primforest"),l,10e-5);

*set boundaries secdforest
v35_secdforest.fx(j,"new") = 0;

v35_secdforest.lo(j,"grow") = 0;
v35_secdforest.up(j,"grow") = pc35_secdforest(j,"grow");
m_boundfix(v35_secdforest,(j,"grow"),l,10e-5);

v35_secdforest.lo(j,"old") = p35_save_secdforest(t,j);
v35_secdforest.up(j,"old") = pc35_secdforest(j,"old");
m_boundfix(v35_secdforest,(j,"old"),l,10e-5);

*set boundaries other land
v35_other.lo(j,"new") = 0;
v35_other.up(j,"new") = Inf;

v35_other.lo(j,"grow") = 0;
v35_other.up(j,"grow") = pc35_other(j,"grow");
m_boundfix(v35_other,(j,"grow"),l,10e-5);

v35_other.lo(j,"old") = p35_save_other(t,j);
v35_other.up(j,"old") = pc35_other(j,"old");
m_boundfix(v35_other,(j,"old"),l,10e-5);

if((ord(t) = 1),
*highest carbon density 1st time step to account for reshuffling
	p35_carbon_density_secdforest(t,j,land35,c_pools) = pm_carbon_density_ac(t,j,"acx",c_pools);
	p35_carbon_density_other(t,j,land35,c_pools) = pm_carbon_density_ac(t,j,"acx",c_pools);
else
*calculate carbon density
	p35_carbon_density_secdforest(t,j,"new",c_pools) = pm_carbon_density_ac(t,j,"ac0",c_pools);
	p35_carbon_density_secdforest(t,j,"grow",c_pools) = m_weightedmean(pm_carbon_density_ac(t,j,ac,c_pools),p35_secdforest(t,j,ac,"before"),(ac_land35(ac,"grow")));
	p35_carbon_density_secdforest(t,j,"old",c_pools) = pm_carbon_density_ac(t,j,"acx",c_pools);
	p35_carbon_density_other(t,j,"new",c_pools) = pm_carbon_density_ac(t,j,"ac0",c_pools);
	p35_carbon_density_other(t,j,"grow",c_pools) = m_weightedmean(pm_carbon_density_ac(t,j,ac,c_pools),p35_other(t,j,ac,"before"),(ac_land35(ac,"grow")));
	p35_carbon_density_other(t,j,"old",c_pools) = pm_carbon_density_ac(t,j,"acx",c_pools);
);

*carbon stock update
pcm_carbon_stock(j,"secdforest",c_pools) = sum(land35, p35_carbon_density_secdforest(t,j,land35,c_pools)*pc35_secdforest(j,land35));
vm_carbon_stock.l(j,"secdforest",c_pools) = pcm_carbon_stock(j,"secdforest",c_pools);
pcm_carbon_stock(j,"other",c_pools) = sum(land35, p35_carbon_density_other(t,j,land35,c_pools)*pc35_other(j,land35));
vm_carbon_stock.l(j,"other",c_pools) = pcm_carbon_stock(j,"other",c_pools);
