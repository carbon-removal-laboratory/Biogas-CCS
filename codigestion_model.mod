# Biogas-CCS Total Cost Model;Codigestion
# Last updated: 12/19/2019
# What has changed?
# 	1) LMOP O&M is now a parameter by facility, which makes more sense than only calculating O&M for only processed LFG.
# 	2) q_feed and q_feedwwtp: now O&M includes WWTP quantities; in codigestion_model, O&M did not include WWTP quantities.
#	3) OUR NEW BASELINE SCENARIO IS: LCFS=$100, RFS = $0.25/D5 ~ $3.25/mmbtu, 45q = $50, rng price = $3
# Update 10/15: We now take into consideration compression emissions.
# THIS VERSION HAS COMPRESSION STRAIGHT TO 15MPA, SO ONLY COUNT CAPITAL COSTS ONCE
# Update 12/19: We now also calculate transport costs directly in the model & account for transport emissions.

# Declaring sets
set FACILITIES;
set LANDFILL within FACILITIES;
set DIGESTER within FACILITIES;
set TYPE;
set CO within TYPE;
set GAS within TYPE;
set INJECTION;
set SOURCE;
set PIPELINES;
set VALID_PAIR within {SOURCE, FACILITIES};
set VALID_SEQ within {FACILITIES, INJECTION};

# Parameters
param ad_ex {FACILITIES};
param rng {FACILITIES};
param pipe_vc {FACILITIES};
param pipe_fc {FACILITIES};
param capacity {FACILITIES};
param lmop_om {LANDFILL};

param rs_dist {VALID_SEQ};
param rs_time {VALID_SEQ};
param fs_dist {VALID_PAIR};
param fs_time {VALID_PAIR};
param per_ton {VALID_PAIR};

param fc_injection {INJECTION};
param vc_injection {INJECTION};
param stor_cap {INJECTION};
param seismic {INJECTION};

param cellulosic {TYPE};
param fstruck_capacity {TYPE};
param vs {TYPE};
param ts {TYPE};
param ton {TYPE};
param biogas_yield {TYPE};
param supply {SOURCE, TYPE}; # total supply of each type at each source
param c_intensity {TYPE};

	# AD Cost parameters
param fc_intercept = 6588000;
param fc_slope1 = 168.71;
param fc_slope2 = 67.167;
param vc_intercept = 530600;
param vc_slope1 = 18.28;
param vc_slope2 = 9.11;
param limit_fc = 121005;
param limit_vc = 133849;

	# Piecewise cost parameters
param upgrade_fc_intercept = 8412;
param upgrade_vc_intercept = 3800;
param upgrade_fc_slope1 = 0.203;
param upgrade_fc_slope2 = 0.078;
param upgrade_vc_slope1 = 0.18;
param upgrade_vc_slope2 = 0.107;
param limit_upgrade_fc = 118785;
param limit_upgrade_vc = 143954;
param comp_fc_intercept = 101500;
param comp_vc_intercept = 3817;
param comp_fc_slope1 = 1.78;
param comp_vc_slope1 = 12.46;
param comp_fc_slope2 = 0.57;
param comp_vc_slope2 = 12.41;
param limit_comp_fc = 113828;
param limit_comp_vc = 107069;
param comp2_fc_intercept = 189400;
param comp2_vc_intercept = 8068;
param comp2_fc_slope1 = 2.79;
param comp2_vc_slope1 = 7.03;
param comp2_fc_slope2 = 1.39;
param comp2_vc_slope2 = 6.98;
param limit_comp2_fc = 100951;
param limit_comp2_vc = 113904;
param injection_fc_intercept = 8412;
param injection_vc_intercept = 66.53;
param injection_fc_slope1 = 0.057;
param injection_vc_slope1 = 0.0011;
param injection_fc_slope2 = 0.019;
param injection_vc_slope2 = 0.0003;
param limit_injection_fc = 110551;
param limit_injection_vc = 100038;
param capture_intercept = 15500;
param capture_slope1 = 7.66;
param capture_slope2 = 6.91;
param limit_capture = 250333;

# Scalars

param ch4_yield = 0.6; #changeable
param rng_price default 1; #$/mmbtu
param seq_credit default 50; #$/t CO2
param cellulosic_waiver default 6.50;
param d5_price default 0; #$/mmbtu
param d3_price = d5_price + cellulosic_waiver; #$/mmbtu
param tipping = 20; #$/ton msw
param ci_baseline = 79.21; #gco2e/mj
param lcfs_price default 0; #$/credit
param irr = 0.1;
param life = 15;
param crf = (irr * (1 + irr)^life)/((1 + irr)^life - 1);
param mj_per_mmbtu = 1055.87;
param m3_to_mmbtu = 0.0368; # natural gas heat value
param m3_to_gco2 = 1832;
param m3_to_tco2 = 0.002;
param co2truck_capacity = 25.67;
param monitoring_cost = 0.1;
param compression_work = 51.78; #kwh/tco2/yr
param compression_work2 = 29.01; #kwh/tco2/yr
param electricity_emissions = 0.0002; #tco2/kwh
param transport_emissions = 161.8; #gco2/ton-mile
param fs_cost_per_mile default 1.08;
param fs_cost_per_hour default 26.11;
param rs_cost_per_mile default 0.8034;
param rs_cost_per_hour default 18.31;
param cost_per_ton default 4.5;

# Decision variables

var ad {FACILITIES} binary; # binary to decide if a facility is active
var trans_mat {VALID_SEQ} binary; #binary to decide where to send co2 
var q_feed {VALID_PAIR, TYPE} >= 0;
var seq {INJECTION} binary; # binary to decide if a sequestration site is used
var q_co2seq {INJECTION} >= 0;
var q_capt{TYPE, FACILITIES} >= 0;
var q_biogas{TYPE, FACILITIES} >= 0;
var q_ch4{TYPE, FACILITIES} >= 0;
	# defined as sum over type under the constraints, since sum within the piecewise objective function is read as nonlinear
var q_co2trans {VALID_SEQ} >= 0;
var q_feedf{FACILITIES} >= 0;
var q_ch4f{FACILITIES} >= 0;
var q_captf{FACILITIES} >= 0;
var q_additional{FACILITIES} >= 0;
var q_feedfwwtp{FACILITIES} >= 0;

# Objective function

minimize Total_cost:

sum {f in DIGESTER} (life * (
	(ad[f] * (fc_intercept * crf + vc_intercept + upgrade_fc_intercept * crf + upgrade_vc_intercept + injection_fc_intercept * crf + injection_vc_intercept + comp_fc_intercept * crf +  comp_vc_intercept + capture_intercept + pipe_fc[f] * crf + pipe_vc[f]) ) + ## all the constants go in here
	<<limit_fc; fc_slope1 * crf , fc_slope2 * crf >> q_feedf[f] + #piecewise estimation of the cost of digester
	<<limit_vc; vc_slope1, vc_slope2>> q_feedfwwtp[f] +
	<<limit_upgrade_fc; (upgrade_fc_slope1 + injection_fc_slope1) * crf + upgrade_vc_slope1 + injection_vc_slope1, (upgrade_fc_slope2 + injection_fc_slope2) * crf + upgrade_vc_slope2 + injection_vc_slope2 >> q_ch4f[f] + #capital cost of upgrading
	<<limit_comp_fc; comp_fc_slope1 * crf + comp_vc_slope1, comp_fc_slope2 * crf + comp_vc_slope2>> q_captf[f] + #capital cost of co2 compression
	<<limit_capture; capture_slope1, capture_slope2 >> q_captf[f] + #annual cost of capture
	lcfs_price * compression_work * electricity_emissions * q_captf[f] + # subtracting emission credits 
	q_ch4f[f] * (- rng_price -  d5_price )
	)
) -
sum {t in CO, f in DIGESTER} ( life * (
		q_ch4[t,f] * mj_per_mmbtu * 0.000001 * (lcfs_price * (ci_baseline - c_intensity[t]) ) )
		)
	+
sum{(s,f) in VALID_PAIR, t in TYPE}  (life * (
		(fs_dist[s,f] * fs_cost_per_mile * 2 + fs_time[s,f]/60 * fs_cost_per_hour * 2) * q_feed[s,f,t]/25  +
		per_ton[s,f] * cost_per_ton * q_feed[s,f,t] )
		)
	+
sum {l in LANDFILL} ( life *  (
		(ad[l] * (upgrade_fc_intercept * crf + upgrade_vc_intercept + injection_fc_intercept * crf + injection_vc_intercept + comp_fc_intercept * crf + comp_vc_intercept + capture_intercept + pipe_fc[l] * crf + pipe_vc[l]) ) + ## all the constants go in here
		lmop_om[l] + #piecewise estimation of the cost of digester
		<<limit_upgrade_fc; (upgrade_fc_slope1 + injection_fc_slope1) * crf + upgrade_vc_slope1 + injection_vc_slope1, (upgrade_fc_slope2 + injection_fc_slope2) * crf + upgrade_vc_slope2 + injection_vc_slope2>> q_ch4f[l] + #capital cost of upgrading
		<<limit_comp_fc; comp_fc_slope1 * crf + comp_vc_slope1, comp_fc_slope2 * crf + comp_vc_slope2>> q_captf[l] + #capital cost of co2 compression
		<<limit_capture; capture_slope1, capture_slope2 >> q_captf[l] +
		lcfs_price * compression_work * electricity_emissions * q_captf[l] + # subtracting emission credits 
		q_ch4f[l] * (- rng_price -  d3_price )
	)
) -
sum {g in GAS, l in LANDFILL} ( life *
		q_ch4[g,l] * mj_per_mmbtu * 0.000001 * (lcfs_price * (ci_baseline - c_intensity[g]) )
		)
	+
sum {i in INJECTION} (life * (
		seq[i] * (fc_injection[i] * crf + seismic[i] * crf +
		vc_injection[i] ) +
		<<limit_comp2_fc; comp2_fc_slope1 * crf + comp2_vc_slope1, comp2_fc_slope2 * crf + comp2_vc_slope2>> q_co2seq[i] + #capital cost of co2 compression
		lcfs_price * compression_work2 * electricity_emissions * q_co2seq[i] # subtracting emission credits 
		- seq_credit * (q_co2seq[i])+
		monitoring_cost * q_co2seq[i] -
		lcfs_price * q_co2seq[i] )
		) +
sum{(f,i) in VALID_SEQ}( life * (
		(rs_cost_per_hour * rs_time[f,i]/60 + rs_cost_per_mile * rs_dist[f,i]) * q_co2trans[f,i]/co2truck_capacity + #transport cost
		lcfs_price * transport_emissions * q_co2trans[f,i] * rs_dist[f,i] / 1000000 )# emissions from transport
	)
;


# Constraints

subject to fs_quant {(s,f) in VALID_PAIR, t in TYPE} : #defining quantity of feedstock to be 0 if ad is not activated
	q_feed[s,f,t] <= supply[s,t] * ad[f];

subject to supply_constraint {s in SOURCE, t in TYPE}:
	sum{(s,f) in VALID_PAIR} q_feed[s,f,t] <= supply[s,t] ;

subject to fs_sum_nowwtp {f in FACILITIES}: #defining q_feedf, in tons
	q_feedf[f] = sum{(s,f) in VALID_PAIR, t in TYPE} (q_feed[s,f,t] * ton[t] - q_feed[s,f,'wwtp'] * ton['wwtp']) ;

subject to fs_sum {f in FACILITIES}:
	q_feedfwwtp[f] = sum {(s,f) in VALID_PAIR, t in TYPE} q_feed[s,f,t] * ton[t];

subject to biogas_definition {t in TYPE, f in FACILITIES}: #defining quantity of biogas
	q_biogas[t,f] = sum{(s,f) in VALID_PAIR} q_feed[s,f,t] * ts[t] * vs[t] * biogas_yield[t] ;

subject to ch4_definition {t in TYPE, f in FACILITIES}: #defining quantity of ch4
	q_ch4[t,f] = sum{(s,f) in VALID_PAIR} q_feed[s,f,t] * ts[t] * vs[t] * biogas_yield[t] * ch4_yield * m3_to_mmbtu;

subject to ch4_sum {f in FACILITIES}: #defining q_ch4f
	q_ch4f[f] = sum{t in TYPE} q_ch4[t,f];

subject to captured_definition {t in TYPE, f in FACILITIES}: #defining quantity of co2 captured
	q_capt[t,f] = sum{ (s,f) in VALID_PAIR} q_feed[s,f,t] * ts[t] *vs[t] * biogas_yield[t] * (1-ch4_yield) * m3_to_tco2;

subject to capt_sum {f in FACILITIES}: #defining q_captf
	q_captf[f] = sum{t in TYPE} q_capt[t,f];
	
subject to co2_trans_constraint {f in FACILITIES}: #amount transported cannot be more than amount captured
	sum{(f,i) in VALID_SEQ} q_co2trans[f,i] = q_captf[f];

subject to co2_seq_constraint {i in INJECTION}: #amount sequestered cannot be more than amount captured
	q_co2seq[i] = sum{(f,i) in VALID_SEQ} q_co2trans[f,i];

subject to co2_capacity_constraint {i in INJECTION}: #defining amount sequestered to be 0 if seq is not activated
	q_co2seq[i] <= stor_cap[i] * seq[i];
	