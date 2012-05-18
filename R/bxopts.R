bxopts <-
function(bs = "ps")
{
  bterms <- c("lasso", "ridge", "bl", "kr", "gk", 
    "gs", "mrf", "ps", "rw1", "rw2", "random", "te", "season")
  if(!is.na(bs <- pmatch(bs, bterms))) {
    bs <- bterms[bs]
    cat("\npossible options for \'bs = \"", bs, "\"\': \n\n", sep = "")
    if(bs %in% c("lasso", "ridge")) {
      cat("---\nOptions for \'MCMC\' models: \n---\n")
      cat("  a, b: options a and b specify the hyperparameters of the  \n",
        "       inverse Gamma prior for the shrinkage parameter lambda. \n",
        "       Default: realvalue, \'a = 0.001\', \'b = 0.001\'. \n\n")
    }
    if(bs == "bl") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat(" gridchoice: How to choose grid points for numerical integration \n",
        "            in Cox and multistate models. May be either \"quantiles\", \n",
        "            \"equidistant\" or \"all\". \n",
        "            Default: \'gridchoice = \"quantiles\"\'. \n\n")
      cat("      tgrid: number of equidistant time points to be used \n",
        "            for numerical integration in Cox and multi-state \n",
        "            models. Only meaningful if gridchoice = \"equidistant\". \n",
        "            Default: integer, \'tgrid = 100\'. \n\n")
      cat("nrquantiles: number of quantiles that are used to define the \n",
        "            grid points for numerical integration in Cox and \n",
        "            multi-state models. First a grid of nrquantiles \n",
        "            quantiles is computed, then the grid for integration \n",
        "            is defined by nrbetween equidistant points between \n",
        "            each quantile. Only meaningful if \n",
        "            gridchoice = \"quantiles\". \n",
        "            Default: integer, \'nrquantiles = 50\'. \n\n")
      cat("  nrbetween: number of points between quantiles that are used \n", 
        "            to define the grid points for numerical integration \n",
        "            in Cox and multi-state models. First a grid of \n", 
        "            nrquantiles quantiles is computed, then the grid for \n",
        "            integration is defined by nrbetween equidistant \n",
        "            points between each quantile. Only meaningful if \n",
        "            gridchoice = \"quantiles\". \n",
        "            Default: integer, \'nrbetween = 5\'. \n\n")
      cat("---\nOptions for \'MCMC\' models: \n---\n")
    }
    if(bs == "kr") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("   knotdata: a matrix with x and y coordinates for the knots. \n\n")
      cat("       full: specifies that all distinct locations should be used as \n",
        "            knots in the  kriging term. \n",
        "            Default: boolean, full = TRUE. \n\n")
      cat("         nu: the smoothness parameter tau of the Matern correlation \n",
        "            function for kriging terms, admissible values are \n",
        "            0.5, 1.5, 2.5, 3.5. \n",
        "            Default: \'nu = 1.5\'. \n\n")
      cat("    maxdist: specifies the value c that is used to \n",
        "            determine the scale parameter p of the Matern correlation \n", 
        "            function for kriging terms. \n",
        "            Default: realvalue, depends on nu, only positive. \n\n")
      cat("       p, q: parameters of the coverage criterion for the space \n",
        "            filling algorithm that determines the knots of a kriging \n",
        "            term. \n",
        "            Default: realvalue, \'p = 20\', \'q = 20\'. \n\n")
      cat("   maxsteps: maximum number of steps to be performed by the space \n",
        "            filling algorithm. \n",
        "            Default: integer, \'maxsteps = 300\'. \n\n")
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")
      cat("---\nOptions for \'MCMC\' models: \n---\n")
    }
    if(bs == "gk") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("   knotdata: a matrix with x and y coordinates for the knots. \n\n")
      cat("       full: specifies that all distinct locations should be used as \n",
        "            knots in the  kriging term. \n",
        "            Default: boolean, full = TRUE. \n\n")
      cat("         nu: the smoothness parameter tau of the Matern correlation \n",
        "            function for kriging terms, admissible values are \n",
        "            0.5, 1.5, 2.5, 3.5. \n",
        "            Default: \'nu = 1.5\'. \n\n")
      cat("    maxdist: specifies the value c that is used to \n",
        "            determine the scale parameter p of the Matern correlation \n", 
        "            function for kriging terms. \n",
        "            Default: realvalue, depends on nu, only positive. \n\n")
      cat("       p, q: parameters of the coverage criterion for the space \n",
        "            filling algorithm that determines the knots of a kriging \n",
        "            term. \n",
        "            Default: realvalue, \'p = 20\', \'q = 20\'. \n\n")
      cat("   maxsteps: maximum number of steps to be performed by the space \n",
        "            filling algorithm. \n",
        "            Default: integer, maxsteps = 300. \n\n")
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")      
      cat("     center: for varying coefficient terms this option requests that \n", 
        "            the effect should be centered to avoid identifiability \n",
        "            problems. \n",
        "            Default: boolean, \'center = FALSE\'. \n\n")
      cat("        map: a map provided as a list of matrix polygons. \n\n") 
      cat("---\nOptions for \'MCMC\' models: \n---\n")
    }
    if(bs == "te") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")  
      cat("---\nOptions for \'MCMC\' models: \n---\n")
      cat("       a, b: the options a and b specify the hyperparameters of the \n",
          "            inverse Gamma prior for the variance tau2. \n",
          "            Default: realvalue, \'a = 0.001\', \'b = 0.001\'. \n\n")
      cat("   min, max: the options min and max define the minimum and maximum \n", 
          "            block sizes of block move updates. In every iteration, \n",
          "            BayesX randomly chooses the block size within this range. \n",
          "            If omitted, the minimum and maximum block sizes are \n", 
          "            automatically determined during the burnin period such that \n",
          "            the average acceptance rate is between 30 and 70 percent. \n",
          "            The specification of minimum and maximum block sizes is only \n",
          "            meaningful in combination with conditional prior proposals \n",
          "            and therefore has no effect for Gaussian responses, \n",
          "            categorical probit models, or if \'proposal = \"iwls\"\' or \n",
          "            \'proposal = \"iwlsmode\"\' is specified. \n",
          "            Default: automatic determination. \n\n") 
      cat("     lambda: provides a starting value for the smoothing parameter \n",
          "            lambda. \n",
          "            Default: realvalue, \'lambda = 0.1\' \n\n")
      cat("   proposal: specifies the type of proposal. \'proposal = \"cp\"\' means \n",
          "            conditional prior proposal, \'proposal = \"iwls\"\' stands for \n",
          "            iteratively weighted least squares (IWLS) proposal and \n",
          "            \'proposal = \"iwlsmode\"\' indicates IWLS based on posterior \n",
          "            mode estimation. \n",
          "            Default: \'proposal = \"iwls\"\'. \n\n")
      cat("    updateW: the option updateW may be used to specify how often the IWLS \n",
          "            weight matrix should be updated. \'updateW = 0\' means never, \n",
          "            \'updateW = 1\' means in every iteration (which is the default), \n",
          "            \'updateW = 2\' means in every second iteration and so on. \n",
          "            Default: integer, \'updateW = 1\'. \n\n")
      cat("   gridsize: the option gridsize can be used to restrict the number of \n",
          "            points (on the x-axis) for which estimates are computed. By \n",
          "            default, estimates are computed at every distinct covariate \n",
          "            value in the data set (indicated by \'gridsize = -1\'). This \n",
          "            may be relatively time consuming in situations where the number \n",
          "            of distinct covariate values is large. If \'gridsize = \"nrpoints\"\' \n",
          "            is specified, estimates are computed on an equidistant grid with \n", 
          "            \"nrpoints\" knots. \n",
          "            Default: integer, \'gridsize = -1\'. \n\n")

    }
    if(bs == "gs") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("        map: a map provided as a list of matrix polygons. \n\n") 
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")
      cat("     center: for varying coefficient terms this option requests that \n", 
        "            the effect should be centered to avoid identifiability \n",
        "            problems. \n",
        "            Default: boolean, \'center = FALSE\'. \n\n") 
      cat("---\nOptions for \'MCMC\' models: \n---\n")
      cat("       a, b: the options a and b specify the hyperparameters of the \n",
          "            inverse Gamma prior for the variance tau2. \n",
          "            Default: realvalue, \'a = 0.001\', \'b = 0.001\'. \n\n")
      cat("   min, max: the options min and max define the minimum and maximum \n", 
          "            block sizes of block move updates. In every iteration, \n",
          "            BayesX randomly chooses the block size within this range. \n",
          "            If omitted, the minimum and maximum block sizes are \n", 
          "            automatically determined during the burnin period such that \n",
          "            the average acceptance rate is between 30 and 70 percent. \n",
          "            The specification of minimum and maximum block sizes is only \n",
          "            meaningful in combination with conditional prior proposals \n",
          "            and therefore has no effect for Gaussian responses, \n",
          "            categorical probit models, or if \'proposal = \"iwls\"\' or \n",
          "            \'proposal = \"iwlsmode\"\' is specified. \n",
          "            Default: automatic determination. \n\n") 
      cat("     lambda: provides a starting value for the smoothing parameter \n",
          "            lambda. \n",
          "            Default: realvalue, \'lambda = 0.1\' \n\n")
      cat("   proposal: specifies the type of proposal. \'proposal = \"cp\"\' means \n",
          "            conditional prior proposal, \'proposal = \"iwls\"\' stands for \n",
          "            iteratively weighted least squares (IWLS) proposal and \n",
          "            \'proposal = \"iwlsmode\"\' indicates IWLS based on posterior \n",
          "            mode estimation. \n",
          "            Default: \'proposal = \"iwls\"\'. \n\n")
      cat("    updateW: the option updateW may be used to specify how often the IWLS \n",
          "            weight matrix should be updated. \'updateW = 0\' means never, \n",
          "            \'updateW = 1\' means in every iteration (which is the default), \n",
          "            \'updateW = 2\' means in every second iteration and so on. \n",
          "            Default: integer, \'updateW = 1\'. \n\n")
      cat("        map: a map provided as a list of matrix polygons. \n\n") 
    }
    if(bs == "random") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")
      cat("---\nOptions for \'MCMC\' models: \n---\n")
      cat("       a, b: the options a and b specify the hyperparameters of the \n",
          "            inverse Gamma prior for the variance tau2. \n",
          "            Default: realvalue, \'a = 0.001\', \'b = 0.001\'. \n\n")
      cat("     lambda: provides a starting value for the smoothing parameter \n",
          "            lambda. \n",
          "            Default: realvalue, \'lambda = 0.1\' \n\n")
      cat("   proposal: specifies the type of proposal. \'proposal = \"cp\"\' means \n",
          "            conditional prior proposal, \'proposal = \"iwls\"\' stands for \n",
          "            iteratively weighted least squares (IWLS) proposal and \n",
          "            \'proposal = \"iwlsmode\"\' indicates IWLS based on posterior \n",
          "            mode estimation. \n",
          "            Default: \'proposal = \"iwls\"\'. \n\n")
      cat("    nofixed: the option nofixed suppresses the estimation of the main \n",
          "            effect for random slopes. \n",
          "            Default: boolean, \'nofixed = FALSE\'. \n\n")

    }
    if(bs == "mrf") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("        map: a map provided as a list of matrix polygons. \n\n") 
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")
      cat("     center: for varying coefficient terms this option requests that \n", 
        "            the effect should be centered to avoid identifiability \n",
        "            problems. \n",
        "            Default: boolean, \'center = FALSE\'. \n\n") 
      cat("---\nOptions for \'MCMC\' models: \n---\n")
      cat("       a, b: the options a and b specify the hyperparameters of the \n",
          "            inverse Gamma prior for the variance tau2. \n",
          "            Default: realvalue, \'a = 0.001\', \'b = 0.001\'. \n\n")
      cat("     lambda: provides a starting value for the smoothing parameter \n",
          "            lambda. \n",
          "            Default: realvalue, \'lambda = 0.1\' \n\n")
      cat("   proposal: specifies the type of proposal. \'proposal = \"cp\"\' means \n",
          "            conditional prior proposal, \'proposal = \"iwls\"\' stands for \n",
          "            iteratively weighted least squares (IWLS) proposal and \n",
          "            \'proposal = \"iwlsmode\"\' indicates IWLS based on posterior \n",
          "            mode estimation. \n",
          "            Default: \'proposal = \"iwls\"\'. \n\n")
      cat("    updateW: the option updateW may be used to specify how often the IWLS \n",
          "            weight matrix should be updated. \'updateW = 0\' means never, \n",
          "            \'updateW = 1\' means in every iteration (which is the default), \n",
          "            \'updateW = 2\' means in every second iteration and so on. \n",
          "            Default: integer, \'updateW = 1\'. \n\n")
      cat("        map: a map provided as a list of matrix polygons. \n\n") 
    }
    if(bs == "ps") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")
      cat("     center: for varying coefficient terms this option requests that \n", 
        "            the effect should be centered to avoid identifiability \n",
        "            problems. \n",
        "            Default: boolean, \'center = FALSE\'. \n\n") 
      cat("---\nOptions for \'MCMC\' models: \n---\n")
      cat("       a, b: the options a and b specify the hyperparameters of the \n",
          "            inverse Gamma prior for the variance tau2. \n",
          "            Default: realvalue, \'a = 0.001\', \'b = 0.001\'. \n\n")
      cat("   min, max: the options min and max define the minimum and maximum \n", 
          "            block sizes of block move updates. In every iteration, \n",
          "            BayesX randomly chooses the block size within this range. \n",
          "            If omitted, the minimum and maximum block sizes are \n", 
          "            automatically determined during the burnin period such that \n",
          "            the average acceptance rate is between 30 and 70 percent. \n",
          "            The specification of minimum and maximum block sizes is only \n",
          "            meaningful in combination with conditional prior proposals \n",
          "            and therefore has no effect for Gaussian responses, \n",
          "            categorical probit models, or if \'proposal = \"iwls\"\' or \n",
          "            \'proposal = \"iwlsmode\"\' is specified. \n",
          "            Default: automatic determination. \n\n") 
      cat("     lambda: provides a starting value for the smoothing parameter \n",
          "            lambda. \n",
          "            Default: realvalue, \'lambda = 0.1\' \n\n")
      cat("   proposal: specifies the type of proposal. \'proposal = \"cp\"\' means \n",
          "            conditional prior proposal, \'proposal = \"iwls\"\' stands for \n",
          "            iteratively weighted least squares (IWLS) proposal and \n",
          "            \'proposal = \"iwlsmode\"\' indicates IWLS based on posterior \n",
          "            mode estimation. \n",
          "            Default: \'proposal = \"iwls\"\'. \n\n")
      cat("    updateW: the option updateW may be used to specify how often the IWLS \n",
          "            weight matrix should be updated. \'updateW = 0\' means never, \n",
          "            \'updateW = 1\' means in every iteration (which is the default), \n",
          "            \'updateW = 2\' means in every second iteration and so on. \n",
          "            Default: integer, \'updateW = 1\'. \n\n")
      cat("   gridsize: the option gridsize can be used to restrict the number of \n",
          "            points (on the x-axis) for which estimates are computed. By \n",
          "            default, estimates are computed at every distinct covariate \n",
          "            value in the data set (indicated by \'gridsize = -1\'). This \n",
          "            may be relatively time consuming in situations where the number \n",
          "            of distinct covariate values is large. If \'gridsize = \"nrpoints\"\' \n",
          "            is specified, estimates are computed on an equidistant grid with \n", 
          "            \"nrpoints\" knots. \n",
          "            Default: integer, \'gridsize = -1\'. \n\n")
      cat(" derivative: if specified, first order derivatives of the function estimate \n",
          "            are computed. \n",
          "            Default: boolean, \'derivative = FALSE\'. \n\n")
      cat("   monotone: defines monotonicity constraints for P-splines. Specifying \n",
          "            \'monotone = \"increasing\"\' yields increasing nonlinear \n",
          "            functions and \'monotone = \"decreasing\" yields decreasing \n",
          "            functions. \n",
          "            Default: \'monotone = \"unrestricted\"\' \n\n")
      cat("contourprob: forces the computation of contour probabilities for P-splines. \n",
          "            For instance, \'contourprob = 4\' specifies that contour \n",
          "            probabilities for difference orders zero to four are computed. \n",
          "            Note that the global simple option \'approx\' may additionally be \n",
          "            specified. In this case the computation of contour probabilities \n",
          "            is based on stochastic approximations for quantiles. \n",
          "            Default: \'contourprob = 4\'. \n\n")
    }
    if(bs == "season") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("     period: period of a seasonal effect. The default \'period = 12\' \n",
        "            corresponds to monthly data. \n",
        "            Default: integer, \'period = 12\'. \n\n")
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")
      cat("     center: for varying coefficient terms this option requests that \n", 
        "            the effect should be centered to avoid identifiability \n",
        "            problems. \n",
        "            Default: boolean, \'center = FALSE\'. \n\n") 
      cat("---\nOptions for \'MCMC\' models: \n---\n")
      cat("       a, b: the options a and b specify the hyperparameters of the \n",
          "            inverse Gamma prior for the variance tau2. \n",
          "            Default: realvalue, \'a = 0.001\', \'b = 0.001\'. \n\n")
      cat("   min, max: the options min and max define the minimum and maximum \n", 
          "            block sizes of block move updates. In every iteration, \n",
          "            BayesX randomly chooses the block size within this range. \n",
          "            If omitted, the minimum and maximum block sizes are \n", 
          "            automatically determined during the burnin period such that \n",
          "            the average acceptance rate is between 30 and 70 percent. \n",
          "            The specification of minimum and maximum block sizes is only \n",
          "            meaningful in combination with conditional prior proposals \n",
          "            and therefore has no effect for Gaussian responses, \n",
          "            categorical probit models, or if \'proposal = \"iwls\"\' or \n",
          "            \'proposal = \"iwlsmode\"\' is specified. \n",
          "            Default: automatic determination. \n\n") 
      cat("     lambda: provides a starting value for the smoothing parameter \n",
          "            lambda. \n",
          "            Default: realvalue, \'lambda = 0.1\' \n\n")
      cat("   proposal: specifies the type of proposal. \'proposal = \"cp\"\' means \n",
          "            conditional prior proposal, \'proposal = \"iwls\"\' stands for \n",
          "            iteratively weighted least squares (IWLS) proposal and \n",
          "            \'proposal = \"iwlsmode\"\' indicates IWLS based on posterior \n",
          "            mode estimation. \n",
          "            Default: \'proposal = \"iwls\"\'. \n\n")
      cat("    updateW: the option updateW may be used to specify how often the IWLS \n",
          "            weight matrix should be updated. \'updateW = 0\' means never, \n",
          "            \'updateW = 1\' means in every iteration (which is the default), \n",
          "            \'updateW = 2\' means in every second iteration and so on. \n",
          "            Default: integer, \'updateW = 1\'. \n\n")
      cat("     period: period of a seasonal effect. The default \'period = 12\' \n",
        "            corresponds to monthly data. \n",
        "            Default: integer, \'period = 12\'. \n\n")
    }
    if(bs == "rw1" || bs == "rw2") {
      cat("---\nOptions for \'REML\' models: \n---\n")
      cat("lambdastart: starting value for the smoothing parameter lambda. \n",
        "            Default: realvalue, positive, \'lambdastart = 10\'. \n\n")
      cat("catspecific: requests that the corresponding effect should be modelled \n",
        "            categoryspecific. Can only be used in cumulative and \n",
        "            sequential models for categorical responses, i.e. with \n", 
        "            response families \"cumlogit\", \"cumprobit\", \"seqlogit\" \n",
        "            and \"seqprobit\". \n",
        "            Default: boolean, \'catspecific = FALSE\' \n\n")
      cat("     center: for varying coefficient terms this option requests that \n", 
        "            the effect should be centered to avoid identifiability \n",
        "            problems. \n",
        "            Default: boolean, \'center = FALSE\'. \n\n") 
      cat("---\nOptions for \'MCMC\' models: \n---\n")
      cat("       a, b: the options a and b specify the hyperparameters of the \n",
          "            inverse Gamma prior for the variance tau2. \n",
          "            Default: realvalue, \'a = 0.001\', \'b = 0.001\'. \n\n")
      cat("   min, max: the options min and max define the minimum and maximum \n", 
          "            block sizes of block move updates. In every iteration, \n",
          "            BayesX randomly chooses the block size within this range. \n",
          "            If omitted, the minimum and maximum block sizes are \n", 
          "            automatically determined during the burnin period such that \n",
          "            the average acceptance rate is between 30 and 70 percent. \n",
          "            The specification of minimum and maximum block sizes is only \n",
          "            meaningful in combination with conditional prior proposals \n",
          "            and therefore has no effect for Gaussian responses, \n",
          "            categorical probit models, or if \'proposal = \"iwls\"\' or \n",
          "            \'proposal = \"iwlsmode\"\' is specified. \n",
          "            Default: automatic determination. \n\n") 
      cat("     lambda: provides a starting value for the smoothing parameter \n",
          "            lambda. \n",
          "            Default: realvalue, \'lambda = 0.1\' \n\n")
      cat("   proposal: specifies the type of proposal. \'proposal = \"cp\"\' means \n",
          "            conditional prior proposal, \'proposal = \"iwls\"\' stands for \n",
          "            iteratively weighted least squares (IWLS) proposal and \n",
          "            \'proposal = \"iwlsmode\"\' indicates IWLS based on posterior \n",
          "            mode estimation. \n",
          "            Default: \'proposal = \"iwls\"\'. \n\n")
      cat("    updateW: the option updateW may be used to specify how often the IWLS \n",
          "            weight matrix should be updated. \'updateW = 0\' means never, \n",
          "            \'updateW = 1\' means in every iteration (which is the default), \n",
          "            \'updateW = 2\' means in every second iteration and so on. \n",
          "            Default: integer, \'updateW = 1\'. \n\n")
    }
  }
}

