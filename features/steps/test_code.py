import subprocess
import time
import re

SERVER_CMD = ["opam", "exec", "--", "dune", "exec", "server"]
CLIENT_CMD = ["opam", "exec", "--", "dune", "exec", "client", "9000", "d", "127.0.0.1", "9002"]
CLIENT1_CMD = ["opam", "exec", "--", "dune", "exec", "client", "9001", "d", "127.0.0.1", "9002"]
CLIENT2_CMD = ["opam", "exec", "--", "dune", "exec", "client", "9010", "d", "127.0.0.1", "9002"]
CLIENT3_CMD = ["opam", "exec", "--", "dune", "exec", "client", "9011", "d", "127.0.0.1", "9002"]

def start_server():
    server_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    return server_proc

def check_connect_client(client):
    assert client.poll() is None

def start_client1(client_cmd=CLIENT_CMD):
    client_proc = subprocess.Popen(client_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    return client_proc

def start_client2(client_cmd=CLIENT1_CMD):
    client_proc = subprocess.Popen(client_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    return client_proc

def start_client3(client_cmd=CLIENT2_CMD):
    client_proc = subprocess.Popen(client_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    return client_proc

def start_client4(client_cmd=CLIENT3_CMD):
    client_proc = subprocess.Popen(client_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    return client_proc

def send_message(client, message):
    client.stdin.write('{}\n'.format(message).encode())
    client.stdin.flush()
    time.sleep(0.1)

def close_client(client):
    client.stdin.write('\\q\n'.encode())
    client.stdin.flush()
    time.sleep(0.1)
    client.kill()
    time.sleep(0.1)
    client.wait()

def find_ports(server):
    ports = []
    while True:
        line = server.stderr.readline()
        if not line:
            break

        match = re.search(rb"join 127.0.0.1 (\d+)", line)
        if match:
            port = match.group(1).decode()
            ports.append(port)
    return ports

def get_port_client_close(client):
    ports = []
    while True:
        line = client.stderr.readline()
        if not line:
            break

        match = re.search(rb"close 127.0.0.1:(\d+)", line)
        if match:
            port = match.group(1).decode()
            ports.append(port)
    return ports

def check_msg(client1):
    k = 0
    while True:
        line = client1.stderr.readline()
        if not line:
            break

        match = re.search(rb"msg \[\d+\.\d+\.\d+\.\d+:\d+\]:", line)
        if match:
            k += 1
    return k

def check_history(client):
    k = 0
    while True:
        line = client.stderr.readline()
        if not line:
            break

        match = re.search(rb"client: \[INFO\] \d+:\d+:\d+ \[\d+\.\d+\.\d+\.\d+:\d+\]:", line)
        if match:
            k += 1
    return k
