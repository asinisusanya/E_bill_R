# Constants
MAX_NAME <- 50

# Function to calculate bill
calculate_bill <- function(units) {
  if (units < 500) {
    charge <- units * 1.00
  } else if (units < 600) {
    charge <- (500 * 1.00) + ((units - 500) * 1.80)
  } else if (units < 800) {
    charge <- (500 * 1.00) + (100 * 1.80) + ((units - 600) * 2.00)
  } else {
    charge <- (500 * 1.00) + (100 * 1.80) + (200 * 2.00) + ((units - 800) * 3.00)
  }
  return(charge)
}

# Function to print bill
print_bill <- function(customer_id, customer_name, units, charge) {
  cat("\n====================================\n")
  cat("         ELECTRICITY BILL            \n")
  cat("====================================\n\n")
  cat(sprintf("Customer ID   : %s\n", customer_id))
  cat(sprintf("Customer Name : %s\n", customer_name))
  cat(sprintf("Units        : %.2f\n", units))
  cat(sprintf("Total Amount : $%.2f\n", charge))
  cat("\nThank you!\n")
  cat("====================================\n")
}

# Main program
main <- function() {
  repeat {
    # Clear the console
    if (.Platform$OS.type == "windows") {
      shell("cls")
    } else {
      system("clear")
    }
    
    cat("\nELECTRICITY BILLING SYSTEM\n")
    cat("====================================\n\n")
    
    # Get customer details
    customer_id <- readline("Enter Customer ID: ")
    customer_name <- readline("Enter Customer Name: ")
    
    # Get units with error handling
    units <- NA
    while (is.na(units)) {
      units_input <- readline("Enter Units Consumed: ")
      units <- tryCatch(
        as.numeric(units_input),
        warning = function(w) NA,
        error = function(e) NA
      )
      if (is.na(units)) {
        cat("Please enter a valid number for units.\n")
      }
    }
    
    # Calculate and print bill
    charge <- calculate_bill(units)
    print_bill(customer_id, customer_name, units, charge)
    
    # Ask for another calculation
    choice <- tolower(readline("\nDo you want to calculate another bill? (y/n): "))
    if (choice != "y") {
      break
    }
  }
  
  cat("\nThank you for using the Electricity Billing System!\n")
}

# Run the program
main()