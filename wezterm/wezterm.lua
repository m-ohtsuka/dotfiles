-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices.
config.font = wezterm.font("PlemolJP Console NF")

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 50

-- or, changing the font size and color scheme.
config.font_size = 17.0
config.color_scheme = "Dracula (Official)"
config.hide_tab_bar_if_only_one_tab = true

-- Finally, return the configuration to wezterm:
return config
