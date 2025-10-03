This is a [Next.js](https://nextjs.org) project bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

## Getting Started

First, run the development server:

```bash
npm run dev
# or
yarn dev
# or
pnpm dev
# or
bun dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

You can start editing the page by modifying `app/page.tsx`. The page auto-updates as you edit the file.

This project uses [`next/font`](https://nextjs.org/docs/app/building-your-application/optimizing/fonts) to automatically optimize and load [Geist](https://vercel.com/font), a new font family for Vercel.

## Backend

El backend del proyecto se encuentra en `src/backend` y corre un servidor FastAPI en `localhost:5000`. A continuación, se detallan los pasos para configurarlo y ejecutarlo correctamente.

### Requisitos

- Python **3.13.4**
- Git (opcional, para clonar el repo)

### Pasos para ejecutar el backend

1. Abre una terminal en la raíz del proyecto y crea un entorno virtual en la carpeta `src/backend`:

   - En Windows:
    ```bash
    py -m venv src/venv
     ```

   - En Linux:
    ```bash
    cd src/backend
    python3 -m venv venv
     ```
   
2. Activa el entorno virtual:

   - En Windows:
    ```bash
   src\venv\Scripts\activate
    ```
   - En Linux:
   ```bash
   source venv/bin/activate
    ```

3. Instala las dependencias necesarias:

   ```bash
   pip install -r requirements.txt
    ```


4. Ejecutar servidor FastAPI:

   ```bash
   cd src/backend
   gunicorn main:app -k uvicorn.workers.UvicornWorker --bind 0.0.0.0:5000 --workers 3 --log-level info
    ```

⚠️ Asegúrate de tener instalado [SWI-Prolog](https://www.swi-prolog.org/build/PPA.html) para que pyswip funcione correctamente.


## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.

