plot_running_mean = function(chains, parameters, actual_params) {
  chains %>%
    mutate(chain = as.factor(chain)) %>%
    select(-accepted) %>%
    gather(key = parameter, value, -iteration, -chain) %>%
    inner_join(actual_params, by = "parameter") %>%
    filter(parameter %in% parameters) %>%
    arrange(parameter, iteration) %>%
    drop_na() %>%
    mutate(m = mean(value), rm = cumsum(value)/iteration) %>%
    ggplot(aes(x = iteration, y = rm, colour = chain)) +
    geom_line() +
    geom_hline(aes(yintercept = m)) +
    facet_wrap(~parameter, ncol = 1, scales = "free_y") +
    geom_hline(aes(yintercept = actual_value), linetype = "dashed", colour = "#ff0000") +
    xlab("Iteration") +
    ylab("Running Mean") +
    theme(legend.position = "none", text = element_text(family = "Georgia"))
}

traceplot = function(chains, parameters, actual_params) {
  chains %>%
    mutate(chain = as.factor(chain)) %>%
    select(-accepted) %>%
    gather(key = parameter, value, -iteration, -chain) %>%
    inner_join(actual_params, by = "parameter") %>%
    filter(parameter %in% parameters) %>%
    arrange(parameter, iteration) %>%
    drop_na() %>%
    ggplot(aes(x = iteration, y = value, colour = chain)) + 
    geom_line() +
    facet_wrap(~parameter, scales = "free_y", ncol = 1) +
    geom_hline(aes(yintercept = actual_value), linetype = "dashed", colour = "#ff0000") +
    theme(legend.position = "none", text = element_text(family = "Georgia"))  
}

plot_density = function(chains, parameters, actual_params) {
  chains %>%
    mutate(chain = as.factor(chain)) %>%
    select(-accepted) %>%
    gather(key = parameter, value, -iteration, -chain) %>%
    inner_join(actual_params, by = "parameter") %>%
    filter(parameter %in% parameters) %>%
    arrange(parameter, iteration) %>%
    drop_na() %>%
    ggplot(aes(x = value)) + 
    geom_histogram(binwidth = 0.05) +
    facet_wrap(~parameter, scales = "free", ncol = 1) +
    geom_vline(aes(xintercept = actual_value), linetype = "dashed", colour = "#ff0000") +
    theme(legend.position = "none", text = element_text(family = "Georgia"))
}
