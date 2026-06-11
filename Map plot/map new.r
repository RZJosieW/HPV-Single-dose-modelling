library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(countrycode)
library(tibble)
library(stringr)
# A. One multicountry/global study
broad_once <- c(
  "DOMINICA",
  "NIUE",
  "BAHRAIN",
  "KUWAIT",
  "OMAN",
  "POLAND",
  "QATAR",
  "ROMANIA",
  "RUSSIAN FEDERATION",
  "SAUDI ARABIA",
  "SLOVAKIA"
)


# B. Multiple multicountry/global studies

broad_repeat <- c(
  "ARGENTINA",
  "ARMENIA",
  "AUSTRALIA",
  "AUSTRIA",
  "BAHAMAS",
  "BARBADOS",
  "BELGIUM",
  "BELIZE",
  "BHUTAN",
  "BOLIVIA (PLURINATIONAL STATE OF)",
  "BOTSWANA",
  "BRAZIL",
  "BRUNEI DARUSSALAM",
  "BULGARIA",
  "CAMEROON",
  "CANADA",
  "CHILE",
  "COLOMBIA",
  "COSTA RICA",
  "CROATIA",
  "CYPRUS",
  "CZECHIA",
  "CÔTE D'IVOIRE",
  "DENMARK",
  "DOMINICAN REPUBLIC",
  "ECUADOR",
  "EL SALVADOR",
  "ESTONIA",
  "ETHIOPIA",
  "FIJI",
  "FINLAND",
  "FRANCE",
  "GAMBIA",
  "GEORGIA",
  "GERMANY",
  "GREECE",
  "GRENADA",
  "GUATEMALA",
  "GUYANA",
  "HONDURAS",
  "HUNGARY",
  "ICELAND",
  "INDONESIA",
  "IRELAND",
  "ISRAEL",
  "ITALY",
  "JAMAICA",
  "JAPAN",
  "KENYA",
  "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
  "LATVIA",
  "LIBERIA",
  "LIBYA",
  "LITHUANIA",
  "LUXEMBOURG",
  "MALAWI",
  "MALAYSIA",
  "MALDIVES",
  "MALTA",
  "MAURITIUS",
  "MEXICO",
  "MICRONESIA (FEDERATED STATES OF)",
  "MYANMAR",
  "NETHERLANDS (KINGDOM OF THE)",
  "NEW ZEALAND",
  "NORTH MACEDONIA",
  "NORWAY",
  "PANAMA",
  "PARAGUAY",
  "PERU",
  "PHILIPPINES",
  "PORTUGAL",
  "REPUBLIC OF KOREA",
  "REPUBLIC OF MOLDOVA",
  "RWANDA",
  "SAINT LUCIA",
  "SAINT VINCENT AND THE GRENADINES",
  "SENEGAL",
  "SINGAPORE",
  "SLOVENIA",
  "SOLOMON ISLANDS",
  "SOUTH AFRICA",
  "SPAIN",
  "SRI LANKA",
  "SURINAME",
  "SWEDEN",
  "SWITZERLAND",
  "THAILAND",
  "TRINIDAD AND TOBAGO",
  "TURKMENISTAN",
  "UGANDA",
  "UNITED ARAB EMIRATES",
  "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND",
  "UNITED REPUBLIC OF TANZANIA",
  "UNITED STATES OF AMERICA",
  "URUGUAY",
  "UZBEKISTAN",
  "ZAMBIA",
  "ZIMBABWE",
  "ANDORRA",
  "ANTIGUA AND BARBUDA",
  "COOK ISLANDS",
  "MARSHALL ISLANDS",
  "MONACO",
  "PALAU",
  "SAINT KITTS AND NEVIS",
  "SAN MARINO",
  "SEYCHELLES",
  "AFGHANISTAN",
  "ALBANIA",
  "ALGERIA",
  "ANGOLA",
  "AZERBAIJAN",
  "BANGLADESH",
  "BELARUS",
  "BENIN",
  "BOSNIA AND HERZEGOVINA",
  "BURKINA FASO",
  "BURUNDI",
  "CAMBODIA",
  "CAPE VERDE",
  "CENTRAL AFRICAN REPUBLIC",
  "CHAD",
  "CHINA",
  "COMOROS",
  "CONGO",
  "CONGO, THE DEMOCRATIC REPUBLIC OF THE",
  "CUBA",
  "DJIBOUTI",
  "EGYPT",
  "EQUATORIAL GUINEA",
  "ERITREA",
  "GABON",
  "GHANA",
  "GUINEA",
  "GUINEA-BISSAU",
  "HAITI",
  "INDIA",
  "IRAN, ISLAMIC REPUBLIC OF",
  "IRAQ",
  "JORDAN",
  "KAZAKHSTAN",
  "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF",
  "KYRGYZSTAN",
  "LEBANON",
  "LESOTHO",
  "MADAGASCAR",
  "MALI",
  "MAURITANIA",
  "MONGOLIA",
  "MONTENEGRO",
  "MOROCCO",
  "MOZAMBIQUE",
  "NAMIBIA",
  "NEPAL",
  "NICARAGUA",
  "NIGER",
  "NIGERIA",
  "PAKISTAN",
  "PAPUA NEW GUINEA",
  "SAMOA",
  "SAO TOME AND PRINCIPE",
  "SERBIA",
  "SIERRA LEONE",
  "SOMALIA",
  "SUDAN",
  "SWAZILAND",
  "SYRIAN ARAB REPUBLIC",
  "TAJIKISTAN",
  "TIMOR-LESTE",
  "TOGO",
  "TONGA",
  "TUNISIA",
  "TURKEY",
  "UKRAINE",
  "VANUATU",
  "VENEZUELA",
  "VIET NAM",
  "YEMEN"
)


# C. One country-level study

one_country_level <- c(
  "Indonesia",
  "Thailand",
  "Canada",
  "Burkina Faso",
  "Rwanda",
  "Brazil",
  "South Africa"
)
# D. Multiple country-level studies

repeated_country_level <- tribble(
  ~country, ~dedicated_count,
  "Uganda", 5,
  "United Kingdom", 2,
  "China", 4,
  "Kenya", 2,
  "India", 5,
  "Vietnam", 3,
  "Nigeria", 3
)

custom_match <- c(
  "BOLIVIA (PLURINATIONAL STATE OF)" = "BOL",
  "BRUNEI DARUSSALAM" = "BRN",
  "CAPE VERDE" = "CPV",
  "CONGO" = "COG",
  "CONGO, THE DEMOCRATIC REPUBLIC OF THE" = "COD",
  "CÔTE D'IVOIRE" = "CIV",
  "IRAN, ISLAMIC REPUBLIC OF" = "IRN",
  "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF" = "PRK",
  "LAO PEOPLE'S DEMOCRATIC REPUBLIC" = "LAO",
  "MICRONESIA (FEDERATED STATES OF)" = "FSM",
  "NETHERLANDS (KINGDOM OF THE)" = "NLD",
  "REPUBLIC OF KOREA" = "KOR",
  "REPUBLIC OF MOLDOVA" = "MDA",
  "RUSSIAN FEDERATION" = "RUS",
  "SAO TOME AND PRINCIPE" = "STP",
  "SWAZILAND" = "SWZ",
  "SYRIAN ARAB REPUBLIC" = "SYR",
  "TURKEY" = "TUR",
  "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND" = "GBR",
  "UNITED REPUBLIC OF TANZANIA" = "TZA",
  "UNITED STATES OF AMERICA" = "USA",
  "VIET NAM" = "VNM",
  "Vietnam" = "VNM",
  "United Kingdom" = "GBR",
  "China" = "CHN"
)

convert_iso3 <- function(x) {
  countrycode(
    x,
    origin = "country.name",
    destination = "iso3c",
    custom_match = custom_match,
    warn = TRUE
  )
}

df_broad_once <- tibble(
  country = broad_once,
  broad_count = 1,
  dedicated_count = 0
)

df_broad_repeat <- tibble(
  country = broad_repeat,
  broad_count = 2,
  dedicated_count = 0
)

df_one_country_level <- tibble(
  country = one_country_level,
  broad_count = 0,
  dedicated_count = 1
)

df_repeated_country_level <- repeated_country_level %>%
  mutate(
    broad_count = 0
  ) %>%
  select(country, broad_count, dedicated_count)

map_data_raw <- bind_rows(
  df_broad_once,
  df_broad_repeat,
  df_one_country_level,
  df_repeated_country_level
) %>%
  mutate(
    country = str_trim(country),
    iso3 = convert_iso3(country)
  )
unmatched <- map_data_raw %>%
  filter(is.na(iso3)) %>%
  distinct(country)

print(unmatched)
map_data <- map_data_raw %>%
  filter(!is.na(iso3)) %>%
  group_by(iso3) %>%
  summarise(
    country_label = first(country),
    broad_count = max(broad_count, na.rm = TRUE),
    dedicated_count = max(dedicated_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_type = case_when(
      
      dedicated_count > 1 ~
        "Multiple country-level studies",
      
      dedicated_count == 1 ~
        "One country-level study",
      
      broad_count > 0 ~
        "Multicountry/global study",
      
      TRUE ~
        "No modelling study"
    )
    
  )

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world_map <- world %>%
  left_join(
    map_data,
    by = c("iso_a3" = "iso3")
  ) %>%
  mutate(
    
    coverage_type = ifelse(
      is.na(coverage_type),
      "No modelling study",
      coverage_type
    ),
    
    broad_count = ifelse(
      is.na(broad_count),
      0,
      broad_count
    ),
    
    dedicated_count = ifelse(
      is.na(dedicated_count),
      0,
      dedicated_count
    ),
    
    coverage_type = factor(
      coverage_type,
      levels = c(
        "No modelling study",
        "Multicountry/global study",
        "One country-level study",
        "Multiple country-level studies"
      )
    )
    
  )

bubble_data <- world_map %>%
  filter(dedicated_count > 0) %>%
  st_make_valid() %>%
  mutate(
    centroid = st_point_on_surface(geometry)
  )

bubble_coords <- st_coordinates(bubble_data$centroid)
bubble_data <- bubble_data %>%
  mutate(
    lon = bubble_coords[,1],
    lat = bubble_coords[,2]
  )
coverage_cols <- c(
  "No modelling study"        = "#F4F4F4",  
  "Multicountry/global study" = "#D9E3E8",  
  "One country-level study"   = "#F2CBBF",  
  "Multiple country-level studies" = "#E6B8A2" 
)
bubble_col <- "#C05640"

p_map <- ggplot() +
  geom_sf(
    data = world_map,
    aes(fill = coverage_type),
    color = "white",       
    linewidth = 0.1       
  ) +
  geom_point(
    data = bubble_data,
    aes(x = lon, y = lat, size = dedicated_count),
    shape = 21,
    fill = bubble_col,
    color = "#FFFFFF",     
    stroke = 0.4,          
    alpha = 0.85        
  ) +
  scale_fill_manual(
    values = coverage_cols,
    name = "Country shading",
    drop = FALSE,
    na.value = "#F4F4F4"
  ) +
  scale_size_continuous(
    name = "Number of country-level studies",
    range = c(2.2, 7),
    breaks = c(1, 2, 3, 4, 5),
    limits = c(1, 5)
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-58, 85),
    expand = FALSE
  ) +
  labs(
    title = ""
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(
      size = 10,
      face = "bold"
    ),
    legend.text = element_text(
      size = 8.8
    ),
    legend.key.size = grid::unit(0.50, "cm"),
    legend.spacing.y = grid::unit(0.12, "cm"),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(color = "grey90")
    ),
    size = guide_legend(
      order = 2,
      override.aes = list(
        fill = bubble_col,
        color = "white",
        alpha = 0.6
      )
    )
  )

print(p_map)