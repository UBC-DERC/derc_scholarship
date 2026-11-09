library(readr)
library(dplyr)
library(igraph)
library(openalexR)
library(rcrossref)

options(openalexR.apikey = "XWAj8b8GNs3RNk8S25ikB3")

ubcAuthors <- c("A5013261223", "A5073166765", "A5103913057", "A5054670630")

coauthors <- list()
coauth_table <- list()

for(i in ubcAuthors) {
  output <- oa_fetch(entity="works", author.id = i, output = "list")
  for(j in output) {
    paper_id <- j$id %||% NA
    paper_title <- j$title %||% NA
    journal <- j$primary_location$raw_source_name %||% NA
    doi <- j$primary_location$doi %||% NA
    if(!is.na(doi)){
      citation <- cr_cn(doi, format="text", "apa")
    } else {
      citation <- NA
    }
    publication_year <- j$publication_year %||% NA
    author_ids <- lapply(j$authorships, function(x)x$author$id %||% NA) %>% unlist()
    countries <- lapply(j$authorships, function(x){
      if(length(x$countries) > 0) {
        return(x$countries[[1]] %||% NA)
        } else {
          return(NA)
        }
      }) %>% unlist()
    if(any(paste0("https://openalex.org/", ubcAuthors) %in% author_ids)) {
      vertices <- data.frame(id = author_ids,
                             country = countries,
                             year = publication_year,
                             journal = journal,
                             title = paper_title,
                             citation = citation,
                             doi = doi) %>%
        filter(!is.na(id)) %>%
        distinct(id, .keep_all=TRUE)
      rows <- data.frame(expand.grid(a = vertices$id, b = vertices$id))
      new_graph <- simplify(graph_from_data_frame(rows, directed = FALSE, vertices = vertices))
      coauthors[[length(coauthors) + 1]] <- new_graph
      coauth_table[[length(coauth_table) + 1]] <- vertices
    }
  }
}

reduce_union <- function(x, y) {
  if(is.null(x)) {
    return(y)
  }
  if(is.null(y)) {
    return(x)
  }
  return(union(x, y))
}

final <- purrr::reduce(coauthors, reduce_union) %>% simplify()

vertices <- coauth_table %>% bind_rows() %>%
  filter(!is.na(country)) %>%
  distinct(id, .keep_all = TRUE)


articles <- coauth_table %>% bind_rows() %>%
  distinct(year, title, doi, .keep_all = TRUE)


canadians <- vertices %>% filter(country == "CA")

ubc_researcher <- vertex.attributes(final)$name %in% paste0("https://openalex.org/", ubcAuthors)
canada_researcher <- vertex.attributes(final)$name %in% canadians$id

vertex_color <- ifelse(canada_researcher, "#9B1B30", "#457D9B")

vertex_size <-  ifelse(vertex.attributes(final)$name == "https://openalex.org/A5073166765", 12, 4)

vertex_size <-  ifelse(vertex.attributes(final)$name %in% paste0("https://openalex.org/", ubcAuthors), 12, 4)

# Full diagram, Leluo always:
plot(final,
     vertex.size = vertex_size,
     vertex.color = vertex_color,
     vertex.label = NA,
     edge.color = "#cfecf7",
     layout = layout_with_kk)

# Split into One graph all collaborations, Canada vs International (coloring)
#  * Lines with consistent coloring
# One graph with Leluo since UBC as same
# One graph/figure with countries colored by collaboration scale


plot(clean_graph,
     vertex.size = vertex_size,
     vertex.label = NA,
     vertex.color = vertex_color,
     edge.color = rgb(.5, 0, 0, .05),
     layout = layout_with_kk)

library(openalexR)

grants = grants[!is.na(grants)]
