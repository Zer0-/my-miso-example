from http.server import SimpleHTTPRequestHandler, HTTPServer
from urllib.request import urlopen, Request
import urllib.parse

class ProxyHandler(SimpleHTTPRequestHandler):
    def do_GET(self):
        # Proxy API requests to Pixabay
        if self.path.startswith('/api'):
            try:
                target_url = "https://pixabay.com" + self.path
                req = Request(target_url, headers={'User-Agent': 'Mozilla/5.0'})
                with urlopen(req) as response:
                    self.send_response(200)
                    self.send_header('Access-Control-Allow-Origin', '*')
                    self.send_header('Content-Type', 'application/json')
                    self.end_headers()
                    self.wfile.write(response.read())
            except Exception as e:
                self.send_error(500, str(e))
        # Serve static files normally
        else:
            super().do_GET()

if __name__ == '__main__':
    server = HTTPServer(('localhost', 8881), ProxyHandler)
    print("Serving on http://localhost:8881 (static files + proxy)")
    server.serve_forever()
