# Shiny entry point of the cyclind interface, used for deployment (Shiny
# Server, Posit Connect) and by the browser tests. Set the environment
# variable CYCLIND_LANGUAGE to "id" for the Indonesian interface (English is
# the default).
cyclind::cyclind_app(Sys.getenv("CYCLIND_LANGUAGE", "en"))
