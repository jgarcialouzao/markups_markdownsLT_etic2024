
mata:
    void GMM_DLW_TL_LL(todo, betas, crit, g, H)
{
	
	PHI=st_data(.,("phi"))
	PHI_LAG=st_data(.,("phi_lag"))
	
	Z=st_data(.,("l_lag", "varinp_lag", "k", "l2_lag", "varinp2_lag", "k2", "l_lagk", "varinp_lagk", "l1varinp1_lag", "c"))
	
	X=st_data(.,("l", "varinp", "k", "l2", "varinp2", "k2", "l1k1", "k1varinp1", "l1varinp1", "c"))
	X_lag=st_data(.,("l_lag", "varinp_lag", "k_lag", "l2_lag", "varinp2_lag", "k2_lag", "l1k1_lag", "k1varinp1_lag", "l1varinp1_lag", "c"))
	C=st_data(.,("c"))
	
	OMEGA=PHI-X*betas'
	OMEGA_lag=PHI_LAG-X_lag*betas'
	OMEGA_lag2 = OMEGA_lag:*OMEGA_lag
	OMEGA_lag3=OMEGA_lag2:*OMEGA_lag
	OMEGA_lag_pol=(C,OMEGA_lag,OMEGA_lag2,OMEGA_lag3)
	g_b = invsym(OMEGA_lag_pol'OMEGA_lag_pol)*OMEGA_lag_pol'OMEGA
	XI=OMEGA-OMEGA_lag_pol*g_b
	W = invsym(Z'Z)/(rows(Z))
	
	crit=(Z'*XI)'*W*(Z'*XI)	
		
}

void DLW_TRANSLOG_LL()
	{
stata("set sortseed 13") 
B = st_matrix("init")
S=optimize_init() 
optimize_init_evaluator(S, &GMM_DLW_TL_LL())
optimize_init_evaluatortype(S,"d0")
optimize_init_technique(S, "nm")
optimize_init_nmsimplexdeltas(S, 0.1)
optimize_init_which(S,"min")
optimize_init_conv_maxiter(S, 1000)
optimize_init_params(S,B) 
betaout=optimize(S)
betaout
st_matrix("beta_dlwtranslog",betaout)
}
end

cap program drop dlw_TL_LL
program dlw_TL_LL, rclass
preserve
sort id year
mata DLW_TRANSLOG_LL()
end



