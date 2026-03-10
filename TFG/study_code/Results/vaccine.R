# load results
library(omopgenerics)
library(here)
library(dplyr)
library(CohortCharacteristics)
library(readr)
library(here)
library(ggplot2)
result <- importSummarisedResult(path = here("vaccine"))
# 
# result <- result |>
#   filterSettings(result_type == "summarise_characteristics") |>
#   filterGroup(cohort_name == "vaccine_record") |>
#   filter(result_id == 7L)

# tableCharacteristics(result )
plotCohortAttrition(result)


plot <- read.csv(here::here("vaccine/plot.csv"))
plot_90 <- read.csv(here::here("vaccine/plot_90.csv"))
plot_dose <- read.csv(here::here("vaccine/plot_dose.csv"))


plot <- plot |>
  mutate(cohort_start_date = as.Date(cohort_start_date))

plot_90 <- plot_90 |>
  mutate(cohort_start_date = as.Date(cohort_start_date))

plot_dose <- plot_dose |>
  mutate(cohort_start_date = as.Date(cohort_start_date))


compare <- plot_dose |> #we check that everything goes as expected<0 rows> 
                        #(o 0- extensión row.names)

dif<-setdiff(plot_dose|>
               distinct(cohort_start_date, n),
               plot_90|>
               distinct(cohort_start_date, n)
             )

plot_in_fin <- plot |>
  filter(n>"5") |>
  group_by(vaccination_campaign) |>
  summarise(
    first_date = min(cohort_start_date),
    last_date  = max(cohort_start_date),
    .groups = "drop"
  )
  
plot_dosef <- plot_dose |> filter(n>"5")

graph <- ggplot(plot_dosef, aes(x = cohort_start_date, y = n_dose, fill = dose)) +
  geom_rect(data = plot_in_fin,
            aes(xmin = first_date, xmax = last_date, ymin = 0, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.15, fill = "grey25") +
  geom_col(position = "stack", width = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "# vaccines", title = "Vaccinations over time (por dosis)")

print(graph)

plot_with_campaign <- plot_dosef |>
  filter(cohort_start_date>as.Date("2023-08-02") & cohort_start_date<as.Date("2026-01-01"))|>
  mutate(vaccination_campaign = case_when(
    cohort_start_date>=as.Date("2023-08-02") & cohort_start_date<=as.Date("2024-01-31") ~ "A_2023",
    cohort_start_date>=as.Date("2024-04-15") & cohort_start_date<=as.Date("2024-06-03") ~ "S_2024",
    cohort_start_date>=as.Date("2024-08-03") & cohort_start_date<=as.Date("2024-12-20") ~ "A_2024",
    cohort_start_date>=as.Date("2025-04-01") & cohort_start_date<=as.Date("2025-06-01") ~ "S_2025",
    cohort_start_date>=as.Date("2025-07-01") & cohort_start_date<=as.Date("2026-01-01") ~ "A_2025"),
  )
graph2 <- ggplot(plot_with_campaign,
       aes(x = cohort_start_date, y = n_dose, fill = dose)) +
  geom_col() +
  facet_grid(vaccination_campaign ~ ., scales = "free_x", space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "# vaccines")

print(graph2)
                             