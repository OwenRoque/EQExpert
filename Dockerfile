# Build
FROM node:20-alpine AS builder

WORKDIR /app

# Copiar dependencias
COPY package*.json ./
RUN npm install --frozen-lockfile

# Copiar todo el codigo
COPY . .

# Compilar Next.js (modo produccion)
RUN npm run build

# Runtime (imagen ligera)
FROM node:20-alpine

WORKDIR /app

# Copiar solo lo necesario del build
COPY --from=builder /app/package*.json ./
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/.next ./.next
COPY --from=builder /app/public ./public
COPY --from=builder /app/next.config.ts ./next.config.ts

EXPOSE 3000

CMD ["npm", "start"]