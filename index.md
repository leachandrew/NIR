
![](Plots/plot_main.png)

**Welcome!** This website provides several visualizations of COVID-19 vaccination progress in Canada based on data collected mainly from [COVID Canada](https://covid19tracker.ca/vaccinationtracker.html) and [Our World in Data](https://ourworldindata.org/covid-vaccinations). The latest federal distribution schedule is [available here](https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/prevention-risks/covid-19-vaccine-treatment/vaccine-rollout.html). The information below was last updated at `r accessed` MDT. For regular progress bar updates, follow <a href="https://twitter.com/CDNVaccineCount" class="uri">@CDNVaccineCount</a> on twitter.

`r paste0(paste0("COVID vaccination update for ",gsub(" 0"," ",format(accessed,"%B %d, %Y")),":\n\n"),
                    ifelse(wday(today)==2,paste0("- Shots reported today *: ",new_doses,"\n"),
                           paste0("- Shots reported today: ",new_doses,"\n")),
                    "- Total shots given: ",total_doses,"\n",
                    "- Age 12+ w/ 1+ Shots: ",age12_vaccinated,"\n",
                    "- Age 18+ w/ 1+ Shots: ",adult_share_experimental," (est)\n",
                    "- Shots per 100 people: ",number(pop_share*100,.1),"\n",
                    "- People fully vaccinated: ",comma(latest$full),"\n",
                    "- Doses distributed: ",comma(latest$dist),"\n",
                    "- Share of distributed doses administered: ",share_dist_used,"\n",
                    "- Inventory: ",inventory," days (at avg pace)\n",
                    "- Days to 75/20 target: ",round(tamdate$totam),"\n",
                    "\n\nSource: https://covid19tracker.ca/vaccinationtracker.html",
                    ifelse(wday(today)==7 | wday(today)==1,"\n\nNote: Weekend updates are incomplete",""),
                    ifelse(wday(today)==2,"\n\nNote: Includes some weekend doses",""))`

<iframe title="COVID Vaccination Progress in Canada" aria-label="table" id="datawrapper-chart-d3PPr" src="https://datawrapper.dwcdn.net/d3PPr/2/" scrolling="no" frameborder="0" style="width: 0; min-width: 100% !important; border: none;" height="601"></iframe><script type="text/javascript">!function(){"use strict";window.addEventListener("message",(function(a){if(void 0!==a.data["datawrapper-height"])for(var e in a.data["datawrapper-height"]){var t=document.getElementById("datawrapper-chart-"+e)||document.querySelector("iframe[src*='"+e+"']");t&&(t.style.height=a.data["datawrapper-height"][e]+"px")}}))}();
</script>

![](Plots/plot_total.png)

`r third_tweet`

![](Plots/pace_national.png)

And finally, dose counts and the share of a population with a shot is informative and important, but the population-level effective protection this provides is lower. One dose is less effective than two, plus there are lags in the vaccine's effect along with numerous uncertainties (especially where the variants are concerned). Based on a two to three week lag, a first-dose efficacy of between 40 to 80 percent, and a second dose efficacy of 95 percent, our current coverage of effective protection is between `r paste0(round(100*filter(plotdata_effective,date==max(date))$low_est),
                        " to ",round(100*filter(plotdata_effective,date==max(date))$high_est))` percent.

![](Plots/plot_effective.png)

For a useful summary of the research literature, see [this report from Public Health Ontario](https://www.publichealthontario.ca/-/media/documents/ncov/covid-wwksf/2021/04/wwksf-vaccine-effectiveness.pdf?la=en).

---

As the share of the population with their first dose is growing large, provinces the number of second doses will accelerate and the interval between jabs shrink. Here's the latest:

![](Plots/plot_first_second.png)

Explore other visualizations by clicking on the appropriate menu item at the top of this page. Enjoy!