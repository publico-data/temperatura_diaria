import os
import sys
import time
from datetime import datetime
from copernicusmarine import subset

# ----------------------
# 1. Get credentials
# ----------------------
# In CI: read from environment variables set by the workflow (no shell quoting issues).
# Locally: fall back to command-line args or interactive prompt.
user = os.environ.get("C_USERNAME") or os.environ.get("COPERNICUSMARINE_SERVICE_USERNAME")
pwd = os.environ.get("C_PWD") or os.environ.get("COPERNICUSMARINE_SERVICE_PASSWORD")

if not user or not pwd:
    if len(sys.argv) >= 3:
        pwd = sys.argv[1]
        user = sys.argv[2]
    elif len(sys.argv) == 1:
        import getpass
        pwd = getpass.getpass("Enter your Copernicus Marine password: ")
        user = input("Enter your username: ")
    else:
        print("Usage: python copernicus_download.py <password> <username>")
        sys.exit(1)

# Expose for the library's internal credential lookup
os.environ["COPERNICUSMARINE_SERVICE_USERNAME"] = user
os.environ["COPERNICUSMARINE_SERVICE_PASSWORD"] = pwd

# ----------------------
# 2. Setup
# ----------------------
output_dir = "data"
os.makedirs(output_dir, exist_ok=True)

today = datetime.now().date()
start_date = datetime.combine(today, datetime.min.time()).replace(hour=8)
end_date = datetime.combine(today, datetime.min.time()).replace(hour=20)


# ----------------------
# 3. Download function (with retry)
# ----------------------
def download_file(dataset_id, bounds, output_file, **kwargs):
    """
    Download subset of data from Copernicus Marine.
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


def download_with_retry(dataset_id, bounds, output_file, max_retries=5, delay=30):
    for attempt in range(1, max_retries + 1):
        try:
            download_file(dataset_id, bounds, output_file)
            return
        except Exception as e:
            if attempt < max_retries:
                print(f"Attempt {attempt} failed: {e}. Retrying in {delay}s...")
                time.sleep(delay)
            else:
                raise


# ----------------------
# 4. Download data for each region
# ----------------------
print(f"Downloading data from {start_date} to {end_date}")

print("Downloading Portugal Continental data...")
download_with_retry(
    "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
    [-9.646719069971319, 36.7143279919028, -7.364793639893833, 41.884404655537004],
    "continente.nc",
)

print("Downloading Madeira data...")
download_with_retry(
    "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
    [-17.31389417651776, 32.37276288911421, -16.23795594074014, 33.14961360086701],
    "madeira.nc",
)

print("Downloading Açores data...")
download_with_retry(
    "cmems_mod_glo_phy_anfc_0.083deg_PT1H-m",
    [-31.38975950285971, 36.822334920430556, -24.94078489348473, 39.827095662618056],
    "acores.nc",
)

print("All downloads completed!")
