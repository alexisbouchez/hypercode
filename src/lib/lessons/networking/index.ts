import type { Chapter, Lesson } from "../types";
import { ipAddresses } from "./data/01-ip-addresses";
import { binaryAndHex } from "./data/02-binary-and-hex";
import { subnets } from "./data/03-subnets";
import { portsAndProtocols } from "./data/04-ports-and-protocols";
import { tcpHandshake } from "./data/05-tcp-handshake";
import { udp } from "./data/06-udp";
import { httpRequestResponse } from "./data/07-http-request-response";
import { dns } from "./data/08-dns";
import { tcpServer } from "./data/09-tcp-server";
import { tcpClient } from "./data/10-tcp-client";
import { eventDrivenServer } from "./data/11-event-driven-server";
import { routingTables } from "./data/12-routing-tables";
import { nat } from "./data/13-nat";
import { loadBalancing } from "./data/14-load-balancing";
import { tlsHandshake } from "./data/15-tls-handshake";

export const networkingChapters: Chapter[] = [
	{ id: "basics", title: "Network Fundamentals" },
	{ id: "protocols", title: "Protocols" },
	{ id: "sockets", title: "Socket Programming" },
	{ id: "advanced", title: "Advanced Networking" },
];

export const networkingLessons: Lesson[] = [
	ipAddresses,
	binaryAndHex,
	subnets,
	portsAndProtocols,
	tcpHandshake,
	udp,
	httpRequestResponse,
	dns,
	tcpServer,
	tcpClient,
	eventDrivenServer,
	routingTables,
	nat,
	loadBalancing,
	tlsHandshake,
];
