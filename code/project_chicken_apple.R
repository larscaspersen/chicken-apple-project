# Here we calculate costs and benefits of the inclusion of chicken into 
# conventional apple orchards####

devtools::install_github("eikeluedeling/decisionSupport")

# define path of input table and others
input_table <- "data/data_chicken_apple.csv"

# function to create global variables from input table
make_variables <- function(est,n=1)
{ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir = .GlobalEnv)}

#create variables to set up function
make_variables(estimate_read_csv(input_table))

#set the path for model outputs
dir.create(evpi_results_folder)

#function to update the tree population after voles killed trees and new young trees were planted
calc_tree_population <- function(mature_tree_population, dead_tree_occurrence, n_year_fruit = 3, 
         n_year_full_harvest = 6){
  
  #create vector of young trees, which have reduced yield
  young_tree <- rep(0, length(dead_tree_occurrence))
  
  for(i in 1:length(dead_tree_occurrence)){
    
    if(dead_tree_occurrence[i] == 0){
      next()
    } else {
      
      #end until which young tree is missing
      j <- i + n_year_full_harvest - 1
      if(j > n_years){j <- n_years}
      
      #point to start add young trees
      k <- i + n_year_fruit
      if(k > n_years){k <- n_years}
      
      #end of vector, either i-1+n_year_full_harvest or n_years
      #substract number of fully mature trees by number of dead trees
      mature_tree_population[i:j] <-  mature_tree_population[i:j] - dead_tree_occurrence[i]
      
      #in first three years trees do not produce anything, so no need to creare variable for that
      
      #from year 3 and including year 6 trees produce apple but lower yield
      young_tree[k:j] <- young_tree[k:j] + dead_tree_occurrence[i]
    }
  } #end loop
  return(list(mature_tree_population = mature_tree_population,
              young_tree = young_tree))
}

  


#Here we are writing the chicken in apple function----

Chicken_Apple_Simulation<- function(){
  
  #####Apple related calculations
  
  #Costs apple orchard----
  
  #set the cost to run machine for one hour
  cost_diesel <- consumption_diesel_hour * diesel_price
  
  #cost to harvest the apples = time it takes to harvest * (wage + diesel_costs)
  cost_harvesting <- vv(yearly_harvesting_hours, var_CV, n=n_years) * (hourly_wage + cost_diesel)
  
  #cost to mow the grass
  cost_mowing <- vv(yearly_mowing_hours, var_CV, n=n_years) * (hourly_wage + cost_diesel)
  
  #cost to remove the weed from the strip
  cost_weeding <- vv(yearly_weeding_hours, var_CV, n=n_years) * (hourly_wage + cost_diesel) + 
    cost_herbicide * orchard_area * share_application_area
  

  
  #### Apple Scab -----
  
  cost_scabcontrol <- vv(yearly_scabcontrol_hours, var_CV, n=n_years) * (hourly_wage + cost_diesel) +
                          cost_fungicide * orchard_area * share_application_area
  
  
  #event of extraordinary apple scab infestation (chance of 25%, should be a variable)
  #changed the 0.3 to a variable in the input table
  #reformulated the equations so that the factor (0.3) is present only once

  #draw which years are high in scab infestation
  intense_scab_year <- chance_event(chance_intense_apple_scab, 1, 0, n = n_years)
  
  #calculate extra costs in these years
  cost_scab_year <- intense_scab_year * cost_scabcontrol * proportion_extracost_apple_scab
  
  #yield modifying vector, 1 = 100 % of yield remains, 1-yieldreduction factor in other cases
  yield_reduction <- rep(1,n_years) - (intense_scab_year * vv(yield_reduction_apple_scab, var_CV = var_CV, n = n_years))
  

  #### Insect control -----
  
  #cost of insect control
  #insects are confused with pheromones, so its a one time application

  cost_insectcontrol <-  vv(yearly_insectcontrol_hours, var_CV, n=n_years) * (hourly_wage + cost_diesel) +
                            cost_insecticide * orchard_area * share_application_area
  
  #event of extraordinary insect infestation
  
  #draw which years are high in scab infestation
  intense_insect_year <- chance_event(chance_intense_insect_infestation, 1, 0, n = n_years)
  
  #calculate extra costs in these years
  cost_insect_year <- intense_insect_year * cost_insectcontrol * proportion_extracost_insect_infestation
  
  #yield modifying vector, 1 = 100 % of yield remains, 1-yieldreduction factor in other cases
  yield_reduction <- yield_reduction - (intense_insect_year * vv(yield_reduction_insect, var_CV = var_CV, n = n_years))
  
  #yearly cost to apply nutrients
  cost_nutrients <- vv(yearly_fertilization_hours, var_CV, n = n_years) * (hourly_wage + cost_diesel) +
                    cost_fertilizer * orchard_area * share_application_area
  
  
  
  #### Voles ----
  
  #vole control costs
  cost_vole_control <- vv(yearly_vole_control_hours, var_CV, n = n_years) * hourly_wage

  #draw which in years occures high vole damage
  intense_vole_year <- chance_event(chance_intense_vole_damage, n = n_years)
  
  #trees dying in response to vole 'attack'
  dead_tree_occurrence <- floor(vv(dead_trees, var_CV, n = n_years)) * intense_vole_year
  
  #cost to replace dead due to vole damage
  cost_replacement_trees <- vv(cost_tree, var_CV = var_CV, n = n_years) * dead_tree_occurrence + 
                            planting_hours_tree * dead_tree_occurrence * hourly_wage
  
  
  

  #yield modifying vector, 1 = 100 % of yield remains, 1-yieldreduction factor in other cases
  yield_reduction <- yield_reduction - (intense_vole_year * vv(yield_reduction_vole, var_CV = var_CV, n = n_years))
  
  
  #Sum up all apple costs costs----
  Costs_apple_production <-   cost_harvesting + cost_mowing + cost_weeding +
                              cost_insectcontrol + cost_insect_year +
                              cost_scabcontrol + cost_scab_year +
                              cost_vole_control + cost_nutrients +
                              cost_replacement_trees
  
  #chicken reduce insect, apple and vole, weed and grass, so we assume that costs linearly decrease
  Cost_apple_production_modified_by_chicken <- cost_harvesting + 
                                               cost_mowing * (1-vv(reduce_grass, var_CV = var_CV, n = n_years)) + 
                                               cost_weeding * (1-vv(reduce_weed, var_CV = var_CV, n = n_years)) + 
                                               (cost_insectcontrol + cost_insect_year) * (1-vv(reduce_pest, var_CV, n = n_years))+
                                               (cost_scabcontrol + cost_scab_year) * (1-vv(reduce_apple_scab, var_CV = var_CV, n = n_years)) + 
                                               (cost_vole_control) * (1-vv(reduce_vole, var_CV, n = n_years)) + 
                                               cost_replacement_trees + cost_nutrients
                                                

    #Revenue apple production----
  
  #vector containing the apple tree population wihtout damage
  mature_tree_population <-rep(orchard_area * trees_per_ha, n_years)
  
  #vector containing apple yield, which can vary from year to year
  apple_yield_per_year <- vv(harvest_tree, var_CV = var_CV, n = n_years)
  
  #have tree population calculated based on vole-induced tree death
  #replaced trees won't yield apples until thir year, full yield after 6 years
  #so the number of trees should be reduced and the trees should be tracked until they bear fruits
  tree_populations <- calc_tree_population(mature_tree_population, dead_tree_occurrence, n_year_fruit,
                                           n_year_full_harvest)
  
  
  #calculate the total apple yield in kg----
  
  #harvest of fully yielding trees
  harvest_mature <- tree_populations$mature_tree_population * apple_yield_per_year
  
  #harvest of 3- 6 year old trees
  harvest_young <- tree_populations$young_tree * apple_yield_per_year * yield_reduction_young_tree
  
  #combined harvest
  harvest_total <- harvest_young + harvest_mature
  
  #reduction in harvest due to vole, insect, vole
  marketable_apple_harvest <- harvest_total * yield_reduction
  
  #sell apples
  Revenue_apple_production <- marketable_apple_harvest * vv(apple_price, var_CV, n = n_years)
  
  #Result_apple_production----
  
  Result_pure_apple <- Revenue_apple_production - Costs_apple_production
  
  Result_apple_with_chicken <- Revenue_apple_production - Cost_apple_production_modified_by_chicken
  
  
  
  
  ###### Chicken
  
  
  #Costs chicken----
  
  #investment of hen housing
  cost_coop <- c(coop_invest * number_hens, rep(0, n_years -1))
  
  #cost of maintaining house, including cost for labour
  cost_maintenance_coop<-c(0,vv(maintenance_coop, var_CV, n=n_years-1)*
                             number_hens)
  
  #coop related costs
  cost_coop_nyears <- cost_coop + cost_maintenance_coop
  
  
  #NOTE: explain rationale behind the fence calculation!
  #investment cost of fence and battery
  cost_fence<-c(((fence_price * 2 * 100 + 2*(number_hens*req_area/100)+
                    battery_price)), 
                rep(0,n_years-1))
  
  #cost to buy new chicken per year
  cost_flock<- number_hens * vv(cost_hen, n = n_years, var_CV =  var_CV)
  
  #cost to insure the chicken (e.g. agains avian influenca)
  cost_insurance <- vv(chicken_insurance, var_CV = var_CV, n = n_years) * number_hens
  
  flock_size <- number_hens * vv(survival_rate, var_CV, n=n_years)
  
  
  #cost related to feeding
  cost_feed<-vv(feed_need, var_CV = var_CV, n = n_years) *
    feed_price * 365 * flock_size
  
  #cost related to bedding
  cost_bedding<-vv(bedding_price, var_CV = var_CV, n = n_years) *
    flock_size
  
  #calculation of daily, weekly and irregular tasks
  daily_cost <- vv(cost_daily, var_CV = var_CV, n = n_years) * 
    flock_size * hourly_wage
  
  #>for the weekly routines
  weekly_cost <- vv(cost_weekly, var_CV = var_CV, n = n_years) * 
    flock_size * hourly_wage
  
  costs_irregular_events <- vv(irregular_cost, var_CV = var_CV, n = n_years) * 
    flock_size * hourly_wage
  
  #>for the veterinary
  costs_vet <- cost_vet_visit + costs_vaccin * number_hens +
    chance_event(chance_extra_vet_visit, cost_vet_visit, 0, n=n_years)  
  
  
  #revenues----
  
  #sell eggs
  revenue_eggs <- vv(egg_price, var_CV, n=n_years) *
    vv(eggs_per_hen, var_CV, n=n_years) * flock_size *
    vv(marketable_share, var_CV, n=n_years)
  
  #sell meat
  revenue_meat <- vv(revenue_hen, var_CV, n=n_years) * flock_size
  
  #insurance pays for forgone revenue from chicken and for their value, so in either case revenue will be the same
  #so it doesnt make a difference if the influenca happens or not for our modelling
  #but you don't get the benfits of the chicken (reduced cost for insect control etc)
  
  #draw event that influenca happens in your 
  occurence_influenca_flock <- chance_event(risk_influenca_flock,n = n_years)
  
  #draw event, that influenca happens in neighbourhood
  occurence_influenca_neighbourhood <- chance_event(risk_influenca_neighbour,n = n_years)
  
  #modify the costs and benefits in case of of avian influenca
  for(i in 1:n_years){
    
    if(occurence_influenca_flock[i] == 1){
      #if avian influenca happens in the flock all the chicken will be killed and 
      #thus costs and beenfits are zero for this year, maybe costs for vet?
      
      #chicken are dead, so no benefit like pest reduction
      costs_irregular_events[i] <- costs_irregular_events[i] * higher_cost_influenca_flock
      
      #chicken are dead, so no benefit like pest reduction
      Cost_apple_production_modified_by_chicken[i] <- Costs_apple_production[i]
      

      
      #insurance_coverage[i] <- cost_flock[i] + revenue_eggs[i] + revenue_meat[i]
    } else if(occurence_influenca_neighbourhood){
      
      #having influenca in neighbourhood increases management costs 
      costs_irregular_events[i] <- costs_irregular_events[i] * higher_cost_influenca_neighbour
      
      #chicken are indoor, so no benefit like pest reduction
      Cost_apple_production_modified_by_chicken[i] <- Costs_apple_production[i]
    }
    
  }
  
  
  #add the different cost and benefit variables up
  
  cost_invest_chicken <- cost_coop_nyears + cost_fence + cost_flock
  
  cost_care_chicken <- cost_feed + cost_bedding + daily_cost +
    weekly_cost + costs_irregular_events + costs_vet + cost_insurance
  
  benefit_direct <- revenue_eggs + revenue_meat
  
  Benefits_chicken <- benefit_direct  
  
  Costs_chicken <- cost_invest_chicken + cost_care_chicken
  
  net_revenue_chicken <- Benefits_chicken - Costs_chicken
  
  
  #add benefit of apple plantation to it
  net_revenue_apple_chicken <- Result_apple_with_chicken + net_revenue_chicken
  
  #calculate NPV for chicken, apple, chicken+apple 
  
  NPV_do_chicken <- discount(net_revenue_apple_chicken - Result_pure_apple, discount_rate, calculate_NPV = T)
  NPV_chicken_solo <- discount(net_revenue_chicken, discount_rate, calculate_NPV = TRUE)
  NPV_apple_solo <- discount(Result_pure_apple, discount_rate, calculate_NPV = TRUE)
  NPV_apple_chicken <- discount(net_revenue_apple_chicken, discount_rate, calculate_NPV = TRUE)

  
  return(list(NPV_apple_chicken = NPV_apple_chicken,
              cashflow_apple_chicken = net_revenue_apple_chicken,
              NPV_apple_only = NPV_apple_solo,
              cashflow_apple_only = Result_pure_apple,
              NPV_chicken_only = NPV_chicken_solo,
              cashflow_chicken_only = net_revenue_chicken,
              NPV_do_chicken = NPV_do_chicken,
              cashflow_do_decision = net_revenue_apple_chicken - Result_pure_apple))
}




n_sim <- 10000



# To get a probabilistic overview we run a Monte Carlo Simulation ####
Chicken_Apple_Simulation <- mcSimulation(estimate_read_csv(input_table),
                                         model_function = Chicken_Apple_Simulation,
                                         numberOfModelRuns = n_sim,
                                         functionSyntax = "plainNames")

# density plot of decsion
jpeg(file="pictures/density_decision_do.jpeg",
     width=700, height=500)
decisionSupport::plot_distributions(mcSimulation_object = Chicken_Apple_Simulation, 
                                    vars = c("NPV_do_chicken"),
                                    old_names = c("NPV_do_chicken"),
                                    new_names = c("NPV include chicken"),
                                    x_axis_name = 'Outcome distribution (in €)',
                                    method = "boxplot_density", 
                                    base_size = 11)
dev.off()

median(Chicken_Apple_Simulation$y$NPV_do_chicken)
quantile(Chicken_Apple_Simulation$y$NPV_do_chicken, probs = c(0.05, 0.95))
sum(Chicken_Apple_Simulation$y$NPV_do_chicken >= 0) / n_sim


jpeg(file="pictures/density_apple-chicken.jpg",
     width=700, height=500)
# This function is to plot the distribution of values ####
decisionSupport::plot_distributions(mcSimulation_object = Chicken_Apple_Simulation, 
                                    vars = c("NPV_apple_chicken",'NPV_apple_only'),
                                    
                                    # You can even add more results here
                                    method = "smooth_simple_overlay", 
                                    x_axis_name = 'Outcome distribution (in €)',
                                    base_size = 11)
dev.off()

#calculate the indirect revenue as apple-chicken - chicken_only - apple_only
Chicken_Apple_Simulation$y$NPV_indirect_revenue <- Chicken_Apple_Simulation$y$NPV_apple_chicken - 
                                                   Chicken_Apple_Simulation$y$NPV_apple_only - 
                                                   Chicken_Apple_Simulation$y$NPV_chicken_only





#calculate the synergy distribution and compare to the direct chicken revenue

jpeg(file="pictures/density_chicken_only.jpeg",
     width=700, height=500)
plot_distributions(mcSimulation_object = Chicken_Apple_Simulation, 
                                    vars = c("NPV_chicken_only"),
                                    # You can even add more results here
                                    method = "boxplot_density", 
                                    x_axis_name = 'Outcome distribution (in €)',
                                    base_size = 11)
dev.off()

quantile(Chicken_Apple_Simulation$y$NPV_chicken_only, probs = c(0.05,0.5, 0.95))
sum(Chicken_Apple_Simulation$y$NPV_chicken_only >= 0) / n_sim




jpeg(file="pictures/density_synergy.jpeg",
     width=700, height=500)
plot_distributions(mcSimulation_object = Chicken_Apple_Simulation, 
                   vars = c("NPV_indirect_revenue"),
                   # You can even add more results here
                   method = "boxplot_density", 
                   x_axis_name = 'Outcome distribution (in €)',
                   base_size = 11)
dev.off()
quantile(Chicken_Apple_Simulation$y$NPV_indirect_revenue, probs = c(0.05,0.5, 0.95))
sum(Chicken_Apple_Simulation$y$NPV_indirect_revenue >= 0) / n_sim



#tried to rebuild the density plot with ggplot
library(ggplot2)
library(scales)
ggplot(Chicken_Apple_Simulation$y)+
  geom_density(aes(x = c(NPV_apple_chicken - NPV_apple_only - NPV_chicken_only), fill = 'chicken indirect revenue'),
               alpha = 0.4)+
  geom_density(aes(x = c(NPV_chicken_only), fill = 'chicken direct revenue'),
               alpha = 0.4)+
  scale_x_continuous(labels = comma)+
  scale_fill_manual(values = c("#009999", "#0000FF"))+
  xlab('Outcome distribution (in €)')+
  ylab('Density estimate')+
  labs(fill = '')+
  theme_bw()



# PLS analysis ----

pls_result <- plsr.mcSimulation(object = Chicken_Apple_Simulation,
                                resultName = 'NPV_do_chicken', ncomp = 1)
input_table <- read.csv("data/data_chicken_apple.csv")

#create and save plot 
jpeg(file="pictures/pls_decision.jpeg",
     width=700, height=500)
plot_pls(pls_result, input_table = input_table, threshold = 0.5)
dev.off()


# cashflow ----


jpeg(file="pictures/cashflow_decision.jpeg",
     width=700, height=500)
plot_cashflow(mcSimulation_object = Chicken_Apple_Simulation, cashflow_var_name = "cashflow_do_decision")
dev.off()


# EVPI ----


# Here we can plot each or many EVPI results
mcSimulation_table <- data.frame(Chicken_Apple_Simulation$x, Chicken_Apple_Simulation$y[c('NPV_do_chicken')])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_do_chicken")

jpeg(file="pictures/evpi_decision_do.jpeg",
     width=700, height=500)
plot_evpi(evpi, decision_vars = ("NPV_do_chicken"))
dev.off()
