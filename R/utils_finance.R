get_scenario_inputs <- function(scenario_name) {
  if (scenario_name == "4 Level 2 Ports") {
    return(list(
      n_ports = 4,
      charger_type = "L2",
      fixed_site_cost = 50000,
      cost_per_port = 12000,
      annual_fixed_opex = 6000,
      variable_opex_per_kwh = 0.03,
      avg_kwh_per_session = 18,
      price_per_kwh = 0.35
    ))
  }
  
  if (scenario_name == "8 Level 2 Ports") {
    return(list(
      n_ports = 8,
      charger_type = "L2",
      fixed_site_cost = 70000,
      cost_per_port = 12000,
      annual_fixed_opex = 9000,
      variable_opex_per_kwh = 0.03,
      avg_kwh_per_session = 18,
      price_per_kwh = 0.35
    ))
  }
  
  if (scenario_name == "2 DC Fast Chargers") {
    return(list(
      n_ports = 2,
      charger_type = "DCFC",
      fixed_site_cost = 120000,
      cost_per_port = 85000,
      annual_fixed_opex = 15000,
      variable_opex_per_kwh = 0.05,
      avg_kwh_per_session = 35,
      price_per_kwh = 0.45
    ))
  }
}

calc_capex <- function(fixed_site_cost, cost_per_port, n_ports) {
  fixed_site_cost + cost_per_port * n_ports
}

calc_annual_energy <- function(daily_sessions, avg_kwh_per_session) {
  daily_sessions * 365 * avg_kwh_per_session
}

calc_annual_revenue <- function(annual_kwh, price_per_kwh) {
  annual_kwh * price_per_kwh
}

calc_annual_opex <- function(annual_fixed_opex, annual_kwh, variable_opex_per_kwh) {
  annual_fixed_opex + annual_kwh * variable_opex_per_kwh
}

calc_npv <- function(capex, annual_net_benefit, years = 10, discount_rate = 0.07) {
  discounted_cashflows <- sum(
    annual_net_benefit / ((1 + discount_rate)^(1:years))
  )
  
  -capex + discounted_cashflows
}

calc_bcr <- function(capex, annual_revenue, annual_opex, years = 10, discount_rate = 0.07) {
  pv_benefits <- sum(
    annual_revenue / ((1 + discount_rate)^(1:years))
  )
  
  pv_costs <- capex + sum(
    annual_opex / ((1 + discount_rate)^(1:years))
  )
  
  pv_benefits / pv_costs
}

run_cost_model <- function(daily_sessions, scenario_name) {
  s <- get_scenario_inputs(scenario_name)
  
  capex <- calc_capex(s$fixed_site_cost, s$cost_per_port, s$n_ports)
  annual_kwh <- calc_annual_energy(daily_sessions, s$avg_kwh_per_session)
  annual_revenue <- calc_annual_revenue(annual_kwh, s$price_per_kwh)
  annual_opex <- calc_annual_opex(s$annual_fixed_opex, annual_kwh, s$variable_opex_per_kwh)
  annual_net_benefit <- annual_revenue - annual_opex
  npv <- calc_npv(capex, annual_net_benefit)
  bcr <- calc_bcr(capex, annual_revenue, annual_opex)
  data.frame(
    Scenario = scenario_name,
    Charger_Type = s$charger_type,
    Ports = s$n_ports,
    Daily_Sessions = daily_sessions,
    Annual_kWh = format(round(annual_kwh, 0), big.mark = ","),
    Annual_Revenue = paste0("$", format(round(annual_revenue, 0), big.mark = ",")),
    Annual_Opex = paste0("$", format(round(annual_opex, 0), big.mark = ",")),
    Capex = paste0("$", format(round(capex, 0), big.mark = ",")),
    NPV = paste0("$", format(round(npv, 0), big.mark = ",")),
    BCR = round(bcr, 2)
  )
}