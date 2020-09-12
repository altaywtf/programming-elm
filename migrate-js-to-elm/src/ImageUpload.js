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

const ImageUpload = ({ onUpload }) => {
  const elmRef = React.useRef();

  const readImages = useCallback(() => {
    const element = document.getElementById("file-upload");
    const files = Array.from(element.files);

    Promise.all(files.map(readImage)).then(onUpload);
  }, [onUpload]);

  useEffect(() => {
    const elm = Elm.ImageUpload.init({
      node: elmRef.current,
    });

    elm.ports.uploadImages.subscribe(readImages);

    return () => {
      elm.ports.uploadImages.unsubscribe(readImages);
    };
  }, []);

  return <div ref={elmRef}></div>;
};

export default ImageUpload;
