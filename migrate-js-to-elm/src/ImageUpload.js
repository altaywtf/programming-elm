import React, { useEffect, useCallback } from "react";
import { Elm } from "./ImageUpload.elm";
import "./ImageUpload.css";

const readImage = (file) => {
  const reader = new FileReader();
  const promise = new Promise((resolve) => {
    reader.onload = (e) => {
      resolve({
        url: e.target.result,
      });
    };
  });

  reader.readAsDataURL(file);
  return promise;
};

const ImageUpload = ({ images, onUpload }) => {
  const elmRef = React.useRef();
  const elmNodeRef = React.useRef();

  const readImages = useCallback(() => {
    const element = document.getElementById("file-upload");
    const files = Array.from(element.files);
    Promise.all(files.map(readImage)).then(onUpload);
  }, [onUpload]);

  useEffect(() => {
    elmRef.current = Elm.ImageUpload.init({
      node: elmNodeRef.current,
    });

    elmRef.current.ports.uploadImages.subscribe(readImages);

    return () => {
      elmRef.current.ports.uploadImages.unsubscribe(readImages);
    };
  }, []);

  useEffect(() => {
    elmRef.current.ports.receiveImages.send(images);
  }, [images]);

  return <div ref={elmNodeRef}></div>;
};

export default ImageUpload;
