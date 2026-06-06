import os
import sys
from datetime import datetime
from copernicusmarine import subset

# ----------------------
# 1. Get credentials from command-line arguments
# ----------------------
if len(sys.argv) < 3:
    if len(sys.argv) == 1:  # Running interactively
        import getpass
        pwd = getpass.getpass("Enter your Copernicus Marine password: ")
        user = input("Enter your username: ")
    else:
        print("Usage: python copernicus_download.py <password> <username>")
        sys.exit(1)
else:
    pwd = sys.argv[1]
    user = sys.argv[2]

# Expose credentials as environment variables so copernicusmarine picks them
# up automatically — avoids calling login() which fails in CI environments.
os.environ.setdefault("COPERNICUSMARINE_SERVICE_USERNAME", user)
os.environ.setdefault("COPERNICUSMARINE_SERVICE_PASSWORD", pwd)

# ----------------------
# 2. Setup
# ----------------------
# Remove stale credentials file to prevent conflicts with env-var auth
credentials_file = os.path.expanduser(
    "~/.copernicusmarine/.copernicusmarine-credentials"
)
if os.path.exists(credentials_file):
    os.remove(credentials_file)

# Create output directory
output_dir = "data"
os.makedirs(output_dir, exist_ok=True)

# Set date range (today 8:00 to today 20:00)
today = datetime.now().date()
start_date = datetime.combine(today, datetime.min.time()).replace(hour=8)
end_date = datetime.combine(today, datetime.min.time()).replace(hour=20)


# ----------------------
# 3. Download function
# ----------------------
def download_file(dataset_id, bounds, output_file, **kwargs):
    """
    Download subset of data from Copernicus Marine
    bounds: [min_lon, min_lat, max_lon, max_lat]
    """
    result = subset(
        dataset_id=dataset_id,
        variables=["thetao"],
        start_datetime=start_date.strftime("%Y-%m-%d %H:%M:%S"),
        end_datetime=end_date.strftime("%Y-%m-%d %H:%M:%S"),
        minimum_longitude=bounds[0],
        maximum_longitude=bounds[2],
        minimum_latitude=bounds[1],
        maximum_latitude=bounds[3],
        **kwargs,
    )

    # Find and rename the downloaded file
    downloaded_files = [
        f for f in os.listdir() if f.startswith("cmems_mod") and f.endswith(".nc")
    ]
    if downloaded_files:
        downloaded_file = downloaded_files[0]
        target_path = os.path.join(output_dir, output_file)
        if os.path.exists(target_path):
            os.remove(target_path)
        os.rename(downloaded_file, target_path)
        print(f"Downloaded and saved: {target_path}")
    else:
        print(f"Warning: No file found for {output_file}")


# ----------------------
# 4. Download data for each region
# ----------------------

print(f"Downloading data from {start_date} to {end_date}")

# Portugal Continental
print("Downloading Portugal Continental data...")
download_file(
    "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
    [-9.646719069971319, 36.7143279919028, -7.364793639893833, 41.884404655537004],
    "continente.nc",
)

# Madeira
print("Downloading Madeira data...")
download_file(
    "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
    [-17.31389417651776, 32.37276288911421, -16.23795594074014, 33.14961360086701],
    "madeira.nc",
)

# Açores
print("Downloading Açores data...")
download_file(
    "cmems_mod_glo_phy_anfc_0.083deg_PT1H-m",
    [-31.38975950285971, 36.822334920430556, -24.94078489348473, 39.827095662618056],
    "acores.nc",
)

print("All downloads completed!")
