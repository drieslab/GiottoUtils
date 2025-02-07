FROM rocker/r-ver

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    pandoc \
    python3-pip \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install CRAN packages
RUN R -e "install.packages(c(\
    'devtools', \
    'remotes', \
    'rcmdcheck', \
    'knitr', \
    'rmarkdown', \
    'covr', \
    'testthat', \
    'lintr', \
    'reticulate', \
    'RColorBrewer', \
    'Matrix', \
    'future', \
    'future.apply', \
    'parallel', \
    'checkmate', \
    'data.table', \
    'gtools', \
    'jsonlite', \
    'lifecycle', \
    'magrittr', \
    'methods', \
    'progressr', \
    'R.utils', \
    'stats' \
    ))"

# Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); \
    BiocManager::install(c('BiocParallel', 'BiocCheck', 'BiocStyle'), ask=FALSE)"

# Setup Python environment
ENV RETICULATE_MINICONDA_PATH=/opt/miniconda
RUN R -e "reticulate::install_miniconda()" && \
    R -e "reticulate::conda_install(packages = 'scipy')"

# Set working directory
WORKDIR /package

# Default command
CMD ["R"]