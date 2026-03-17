# =====================================================
# DEPLOYMENT SCRIPT FOR AQUALENS
# Save this as "deploy.R" and run it
# =====================================================

# Set working directory
setwd("E:/R studio project/Water Quality")
cat("Working directory set to:", getwd(), "\n")

# Check files
cat("\nFiles in directory:\n")
print(list.files())

# Remove old deployment cache
if (dir.exists("rsconnect")) {
  unlink("rsconnect", recursive = TRUE)
  cat("\n✓ Removed old rsconnect folder\n")
}

# Load package
library(rsconnect)
cat("✓ rsconnect package loaded\n")

# Set account info
rsconnect::setAccountInfo(
  name = 'salek',
  token = '3C7F71726FEEF678FC508A5036F9D186',
  secret = 'z+ghmoiWynxe6SNj+7XlKgzXbd8zjpNYpORVgbgr'
)
cat("✓ Account credentials set\n")

# Deploy
cat("\n🚀 Deploying to shinyapps.io...\n")
rsconnect::deployApp(
  appDir = getwd(),
  appName = "aqualens",
  account = "salek",
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("\n✅ Deployment complete!\n")
cat("   App URL: https://salek.shinyapps.io/aqualens/\n")