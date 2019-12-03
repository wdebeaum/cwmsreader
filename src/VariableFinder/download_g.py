import requests
import time

def download_file_from_google_drive(id, destination):
    def get_confirm_token(response):
        for key, value in response.cookies.items():
            if key.startswith('download_warning'):
                return value

        return None

    def save_response_content(response, destination):
        CHUNK_SIZE = 32768
        start_time = time.time()
        with open(destination, "wb") as f:
            try:
                total_size = int(response.headers.get('content-length'))
                print('file total size :',total_size)
            except TypeError:
                total_size = 259672272
            count = 1
            for chunk in response.iter_content(CHUNK_SIZE):
                if chunk: # filter out keep-alive new chunks
                    duration = time.time() - start_time
                    progress_size = int(count * CHUNK_SIZE)
                    if duration == 0:
                        duration = 0.1
                    speed = int(progress_size / (1024 * duration))
                    percent = int(count * CHUNK_SIZE * 100 / total_size)
                    sys.stdout.write("\r...%d%%, %d MB, %d KB/s, %d seconds passed" %
                            (percent, progress_size / (1024 * 1024), speed, duration))
                    f.write(chunk)
                    count += 1
            print("\ndownload completed")

    URL = "https://docs.google.com/uc?export=download"

    session = requests.Session()

    response = session.get(URL, params = { 'id' : id }, stream = True)
    token = get_confirm_token(response)

    if token:
        params = { 'id' : id, 'confirm' : token }
        response = session.get(URL, params = params, stream = True)

    save_response_content(response, destination)    


if __name__ == "__main__":
    import sys
    if len(sys.argv) is not 3:
        print("Usage: python google_drive.py drive_file_id destination_file_path")
    else:
        # TAKE ID FROM SHAREABLE LINK
        file_id = sys.argv[1]
        # DESTINATION FILE ON YOUR DISK
        destination = sys.argv[2]
        download_file_from_google_drive(file_id, destination)
