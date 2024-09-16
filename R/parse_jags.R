

parse_jags_dist = function(text) {
  stopifnot(vek::is_chr_vec_xb1(text))

  dist_info = get_jags_dist_info()
  supported_distributions = names(dist_info)

  # TODO trimws() first?
  if (!any(startsWith(text, supported_distributions), na.rm = FALSE))
    return(NULL)

  dist_name = strsplit_(text, "\\(")[1L]
  dist_info_ = dist_info[[dist_name]]

  obj = do.call(parse_call3, list(text, dist_info_$name, dist_info_$params))
  # TODO validate using rjags?

  return(obj)
}


# TODO check size of numbers
is_jags_number = function(x) {
  stopifnot(vek::is_chr_vec_x(x))
  pattern = "^[+-]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:(E|e)[+-]?\\d+)?$"
  return(grepl(pattern, x))
}


# TODO sign?
# TODO scientific notation
is_jags_integer = function(x) {
  stopifnot(vek::is_chr_vec_x(x))
  pattern = "^\\d+$"
  return(grepl(pattern, x))
}


# TODO deal with invalid JAGS syntax
parse_call = function(x) {
  stopifnot(vek::is_chr_vec_x1(x))

  trimws_ = \(k) trimws(k, "both", "[ \\t\\r\\n]")
  parts1 = strsplit_(x, "\\(")
  parts2 = strsplit_(parts1[2L], ")")  # TODO deal with truncation

  dist_name = parts1[1L]
  args_str = strsplit_(parts2[1L], ",") |>
    trimws_()

  # Check whether the arguments are valid numbers.
  is_ok_args = is_jags_number(args_str)
  if (!all(is_ok_args, na.rm = FALSE))
    return(NULL)

  # Convert the string values to numerics.
  args = lapply(args_str, parse_jags_number)

  trunc_args = NULL
  if (grepl("T", x, fixed = TRUE)) { # TODO
    parts3 = strsplit_(x, "T")
    parts4 = parts3[2L] |> trimws_()
    if (!startsWith(parts4, "(") || !endsWith(parts4, ")")) {
      return(NULL)
    }

    parts4 = gsub("[\\()]", "", parts4) |> # Remove first and last brackets
      trimws("both", "[ \\t\\r\\n]")

    if (str_count(parts4, ",") != 1L) {
      return(NULL)
    }

    args_str2 = strsplit_(parts4, ",") |> trimws_()

    # Case 'T(,)'
    if (parts4 == ",") {
      trunc_args = NULL
    }
    # Case 'T(x, y)'
    else if (length(args_str2) == 2L && vek::is_chr_vec_xb(args_str2)) {
      is_ok_args2 = is_jags_number(args_str2)
      if (!all(is_ok_args2, na.rm = FALSE)) {
        return(NULL)
      }

      trunc_args = lapply(args_str2, parse_jags_number)
    # Case 'T(x,)' or 'T(, y)'
    } else if (length(args_str2) == 2L) {
      trunc_args = list(NULL, NULL)
      trunc_index = if (args_str2[2L] == "") 1L else 2L
      trunc_args[[trunc_index]] = parse_jags_number(args_str2[trunc_index])
    }
  }

  return(list(
    name = dist_name,
    args = args,
    truncation = trunc_args
  ))
}


parse_jags_number = function(x) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(x) # TODO check if is_jags_number?
  })

  if (is_jags_integer(x))
    return(as.integer(x))
  else
    return(as.double(x)) # TODO
}


parse_call2 = function(x, name, num_args) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    vek::is_chr_vec_xb1(name)
    vek::is_int_vec_x1(num_args)
  })

  obj = parse_call(x)
  if (is.null(obj))
    return(NULL)
  if (obj$name != name)
    return(NULL)
  if (length(obj$args) != num_args)
    return(NULL)
  return(obj)
}


parse_call3 = function(x, name, params) {
  stopifnot(exprs = {
    vek::is_chr_vec_x1(x)
    vek::is_chr_vec_xb1(name)
    vek::is_chr_vec_xb(params)
    length(params) > 0L
    length(unique(params)) == length(params)
  })

  obj = parse_call2(x, name, length(params))
  if (is.null(obj))
    return(NULL)

  args = obj$args
  names(args) = params
  content = list(name = obj$name, args = args)

  if (is.null(obj$truncation))
    content = c(content, list(truncation = NULL))
  else
    content$truncation = obj$truncation

  return(structure(content, class = "pcj_jags_dist"))
}


new__jags_dist_info = function(name, params) {
  stopifnot(exprs = {
    vek::is_chr_vec_nxb1(name)
    vek::is_chr_vec_nxb(params)
    length(params) > 0L
    length(unique(params)) == length(params)
    all(params != "truncation", na.rm = FALSE)
  })

  return(list(name = name, params = params))
}


get_jags_dist_info = function() {
  list(
    dbeta =      new__jags_dist_info("dbeta", c("a", "b")),
    dchisqr =    new__jags_dist_info("dchisqr", "k"),
    dexp =       new__jags_dist_info("dexp", "lambda"),
    df =         new__jags_dist_info("df", c("n", "m")),
    dgamma =     new__jags_dist_info("dgamma", c("r", "lambda")), # TODO
    dlogis =     new__jags_dist_info("dlogis", c("mu", "tau")),
    dlnorm =     new__jags_dist_info("dlnorm", c("mu", "tau")),
    dnorm =      new__jags_dist_info("dnorm", c("mu", "tau")),
    dt =         new__jags_dist_info("dt", c("mu", "tau", "k")), # TODO
    dweib =      new__jags_dist_info("dweib", c("v", "lambda")), # TODO
    dunif =      new__jags_dist_info("dunif", c("a", "b"))
  )
}

#dbern =      new__jags_dist_info("dbern", "p"),
#dbin =       new__jags_dist_info("dbin", c("p", "n")),
#ddexp =      new__jags_dist_info("ddexp", c("mu", "tau")),
#dgen.gamma = new__jags_dist_info("dgen.gamma", c("r", "lambda", "b")),
#dhyper =     new__jags_dist_info("dhyper", c("n1", "n2", "m1", "psi")) # TODO
#dnegbin =    new__jags_dist_info("dnegbin", c("p", "r")),
#dnchisqr =   new__jags_dist_info("dnchisqr", c("k", "delta")),
#dpar =       new__jags_dist_info("dpar", c("alpha", "c")),
#dpois =      new__jags_dist_info("dpois", "lambda"),
#dwish =      new__jags_dist_info("dwish", c("R", "k")),
#dnt =        new__jags_dist_info("dnt", c("mu", "tau", "df")),
#dcat =       new__jags_dist_info("dcat", "pi"),

