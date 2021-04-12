library(tidyverse)
library(igraph)
library(R.utils)

# Load links, list of pages, list of redirects
links <- read.csv("page_refs_clean.csv", sep=",")
pages <- read.csv("page_titles.csv", sep=",")
redirects <- read.csv("redirects.csv", sep=",")

clean_links <- links

# Clean links that were in namespaces, subpages
clean_links <- clean_links[grep(":", clean_links$From, invert = TRUE) , ]
clean_links <- clean_links[grep(":", clean_links$To, invert = TRUE) , ]
clean_links <- clean_links[grep("/", clean_links$From, invert=TRUE), ]
clean_links <- clean_links[grep("/", clean_links$To, invert=TRUE), ]

# Clean links that directed to a section (and bring it to the main page)
for(i in 1:nrow(clean_links)){
  clean_links[i, ] <- gsub("#.*", "", clean_links[i, ])
}

# Clean links that linked to a non-existent page
clean_links <- clean_links %>% filter(To %in% pages$Title)

# Clean links that were redirects 
# (and then make each reference of that redirect into their correct page)
clean_links$To <- ifelse(clean_links$To %in% redirects$From,
                         clean_links$To[match(clean_links$To, redirects$From)],
                         clean_links$To)

# Clean links that were redirects
clean_links <- clean_links[!(clean_links$From %in% redirects$From),]

# Clean links that went to NULL or "" (self-directing)
clean_links <- clean_links %>% filter(To != "")

# Clean self-directing links
clean_links <- clean_links %>% filter(From != To)

# Capitalize ALL links
clean_links$To <- capitalize(clean_links$To)

# Only get unique clean links
clean_links <- unique(clean_links)

g <- graph.data.frame(clean_links, directed=TRUE, vertices=unique_pages)
print(g)

plot.igraph(g, size=2, color="teal", shape="circle", label=)

write.csv(clean_links,"cleaner_links.csv", row.names = FALSE)
