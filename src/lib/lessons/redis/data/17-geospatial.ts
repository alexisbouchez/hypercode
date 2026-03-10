import type { Lesson } from "../../types";

export const geospatial: Lesson = {
  id: "geospatial",
  title: "Geospatial Indexes",
  chapterId: "advanced",
  content: `## Redis Geospatial Indexes

Redis can store geographic coordinates (longitude/latitude) and efficiently query them by distance. Under the hood, geospatial data is stored in a sorted set using a geohash encoding, but Redis provides dedicated commands for spatial queries.

### GEOADD — Store Locations

\`\`\`
GEOADD restaurants -73.985428 40.748817 "Empire State Diner"
GEOADD restaurants -73.968285 40.763710 "Central Park Cafe"
GEOADD restaurants -74.006015 40.714270 "Wall Street Grill"
\`\`\`

Format: \`GEOADD key longitude latitude member\`. You can add multiple members in a single command.

> **Note:** Longitude comes before latitude in Redis commands (opposite of Google Maps' lat/lng order).

### GEODIST — Distance Between Two Points

\`\`\`
GEODIST restaurants "Empire State Diner" "Central Park Cafe" km
\`\`\`

Supported units: \`m\` (meters, default), \`km\`, \`mi\` (miles), \`ft\` (feet).

### GEOPOS — Get Coordinates

\`\`\`
GEOPOS restaurants "Empire State Diner"
\`\`\`

Returns the longitude and latitude of the stored member.

### GEOSEARCH — Find Nearby Locations

\`\`\`
GEOSEARCH restaurants FROMLONLAT -73.985 40.749 BYRADIUS 3 km ASC
\`\`\`

Finds all members within 3 km of the given coordinates, sorted by distance (ASC = nearest first, DESC = farthest first).

You can also search from an existing member:

\`\`\`
GEOSEARCH restaurants FROMMEMBER "Empire State Diner" BYRADIUS 5 km ASC
\`\`\`

### Real-World Use Cases

**Ride-sharing (find nearby drivers):**
\`\`\`
GEOADD drivers -73.9857 40.7484 "driver:101"
GEOADD drivers -73.9712 40.7831 "driver:102"
GEOADD drivers -73.9903 40.7527 "driver:103"

GEOSEARCH drivers FROMLONLAT -73.985 40.750 BYRADIUS 2 km ASC COUNT 3
\`\`\`

**Store locator:**
\`\`\`
GEOADD stores -122.4194 37.7749 "SF Downtown"
GEOADD stores -122.4089 37.7837 "Union Square"
GEOADD stores -122.4332 37.7899 "Marina"

GEOSEARCH stores FROMLONLAT -122.42 37.78 BYRADIUS 5 km ASC
\`\`\`

**Delivery radius check:**
\`\`\`
GEOADD warehouse -73.985 40.748 "main-warehouse"
GEOADD customers -73.970 40.760 "customer:1"

GEODIST warehouse "main-warehouse" "customer:1" km
# If distance > 10km, charge delivery fee
\`\`\`

### How It Works Internally

Redis encodes each longitude/latitude pair into a **52-bit geohash** and stores it as the score in a sorted set. This means:

- \`ZRANGE\`, \`ZCARD\`, and other sorted set commands also work on geo keys
- Memory usage is the same as a sorted set
- Range queries use the sorted set's O(log N) lookup

### Your Task

Build a "find nearby coffee shops" feature:
1. GEOADD \`shops\` with these locations:
   - \`-73.985130 40.758896 "Times Square Bean"\`
   - \`-73.968285 40.763710 "Park Ave Coffee"\`
   - \`-73.993500 40.750600 "Hudson Yards Brew"\`
   - \`-74.006015 40.714270 "FiDi Espresso"\`
2. Find all shops within 4 km of longitude \`-73.985\`, latitude \`40.755\`, sorted nearest first
3. Get the distance between "Times Square Bean" and "FiDi Espresso" in km`,

  starterCode: `# Add coffee shops with coordinates (all in one command)
GEOADD shops -73.985130 40.758896 "Times Square Bean" -73.968285 40.763710 "Park Ave Coffee" -73.993500 40.750600 "Hudson Yards Brew" -74.006015 40.714270 "FiDi Espresso"

# Find shops within 4 km of Times Square area
GEOSEARCH shops FROMLONLAT -73.985 40.755 BYRADIUS 4 km ASC

# Distance from Times Square to FiDi
GEODIST shops "Times Square Bean" "FiDi Espresso" km`,

  solution: `GEOADD shops -73.985130 40.758896 "Times Square Bean" -73.968285 40.763710 "Park Ave Coffee" -73.993500 40.750600 "Hudson Yards Brew" -74.006015 40.714270 "FiDi Espresso"
GEOSEARCH shops FROMLONLAT -73.985 40.755 BYRADIUS 4 km ASC
GEODIST shops "Times Square Bean" "FiDi Espresso" km`,

  tests: [
    {
      name: "GEOADD added 4 shops",
      expected: '{"type":"contains","value":"(integer) 4"}',
    },
    {
      name: "Times Square Bean found in search",
      expected: '{"type":"contains","value":"\\"Times Square Bean\\""}',
    },
    {
      name: "distance is calculated",
      expected: '{"type":"contains","value":"\\""}',
      code: `GEOADD shops -73.985130 40.758896 "Times Square Bean" -74.006015 40.714270 "FiDi Espresso"
---VALIDATE---
GEODIST shops "Times Square Bean" "FiDi Espresso" km`,
    },
  ],
};
