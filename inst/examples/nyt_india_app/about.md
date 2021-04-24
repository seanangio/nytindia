### About the Data

The data for this project comes from the <a href="https://developer.nytimes.com/docs/articlesearch-product/1/overview" target="_blank">New York Times' Article Search API</a>. The query specified that results include:

- an "India" location keyword tag,
- the document type "article",
- and the source be one of "The New York Times", "International New York Times", or "International Herald Tribune".

The API allows queries dating as far back as 1851, but the earliest record matching my query dates to 1860. My query stretched from this date until the end of 2020.

All of the R scripts used to produce this project -- from the initial data query to the final app -- can be found in <a href="https://github.com/seanangio/nytindia" target="_blank"> Github</a>.

### About the Project

This app offers a number of different ways to visualize and investigate 160 years of India's coverage in The New York Times.

- The **Table** tab is place to read individual headlines, abstracts, and leads. You can also download data exports.
- The **Counts** tab provides simple bar charts to understand the contents of most variables in the dataset.
- The **Timeline** tab plots the count of articles over time.
- The **Maps** tab shows the geographic distribution of location keywords.
- The **Keyword Pairs** tab explores the frequency at which two keywords are found in the same article.

Two package vignettes support the project:

- You can find my own analysis of the data in the <a href="https://seanangio.github.io/nytindia/articles/analysis.html" target="_blank"> Analysis</a> vignette. 
- Those more interested in the technical details of querying, preparing, and visualizing the data with R should see the <a href="https://seanangio.github.io/nytindia/articles/technical-details.html" target="_blank"> Technical Details</a> vignette.

### About the Filters

I'd suggest first familiarizing yourself with the available filters:

- You can narrow your search by date or specific governments.
- You can select a certain piece of the newspaper, such as the Foreign or Editorial Desks. The dropdown options are sorted in descending order of frequency.
- You can filter for specific keywords or return only articles matching a certain word found in the headline, abstract, or lead paragraph.
- As you'll see in the **Table** tab, the keywords for every article are ranked in order of importance. You can require that the "India" keyword is of a certain rank to ensure a minimum level of India's centrality to an article.
- If interested in only the print edition, you can also exclude digital-only results, such as blogs -- most notably, the "India Ink" blog that ran from 2011 to 2014.

Hover over the information icons for more details about a particular filter.

### About the Author

Hi! I'm Sean Angiolillo. You can find my other data visualization projects at <a href="https://sean.rbind.io/" target="_blank"> https://sean.rbind.io/</a> or follow me on <a href="https://twitter.com/seanangiolillo" target="_blank"> Twitter</a>.
