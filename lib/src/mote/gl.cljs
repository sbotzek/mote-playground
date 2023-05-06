(ns mote.gl
  (:require
   [cljs.core.async :refer [go <!]]
   [mote.assetdb :as assetdb]))

(defrecord Shader [gl program attributes uniforms])

(defn- load-shader-source
  "Loads the source for a shader."
  [gl type source]
  (let [shader (.createShader gl type)]
    (.shaderSource gl shader source)
    (.compileShader gl shader)
    (when-not (.getShaderParameter gl shader gl.COMPILE_STATUS)
      (let [info (.getShaderInfoLog gl shader)]
        (.deleteShader gl shader)
        (throw (ex-info info {}))))
    shader))

(defn- shader-program-attribute-locations
  "Returns the map of the shader program's attribute locations."
  [program gl]
  (reduce (fn [sofar idx]
            (let [info (.getActiveAttrib gl program idx)]
              (if info
                (assoc sofar (.-name info)
                       (.getAttribLocation gl program (.-name info)))
                sofar)))
          {}
          (range (.getProgramParameter gl program gl.ACTIVE_ATTRIBUTES))))

(defn- shader-program-uniform-locations
  "Returns the map of the shader program's uniform locations."
  [program gl]
  (reduce (fn [sofar idx]
            (let [info (.getActiveUniform gl program idx)]
              (if info
                (assoc sofar (.-name info)
                       (.getUniformLocation gl program (.-name info)))
                sofar)))
          {}
          (range (.getProgramParameter gl program gl.ACTIVE_UNIFORMS))))

(defn load-shader
  "Loads a shader (program)."
  ([gl shader-source]
   (let [vertex-shader-source (str "#define VERTEX;\n" shader-source)
         fragment-shader-source (str "#define FRAGMENT;\n" shader-source)]
     (load-shader gl vertex-shader-source fragment-shader-source)))

  ([gl vs-source fs-source]
   (let [vertex-shader (load-shader-source gl gl.VERTEX_SHADER vs-source)
         fragment-shader (load-shader-source gl gl.FRAGMENT_SHADER fs-source)
         program (.createProgram gl)]
     (.attachShader gl program vertex-shader)
     (.attachShader gl program fragment-shader)
     (.linkProgram gl program)
     (when-not (.getProgramParameter gl program gl.LINK_STATUS)
       (throw (ex-info (.getProgramInfoLog gl program) {})))
     (map->Shader {:gl gl
                   :program program
                   :attributes (shader-program-attribute-locations program gl)
                   :uniforms (shader-program-uniform-locations program gl)}))))

(defn shader-uniform-matrix4x4-by-location
  [^Shader shader uniform-location value]
  (.uniformMatrix4fv (.-gl shader) uniform-location false value)
  shader)

(defn shader-uniform-matrix4x4
  [^Shader shader uniform-name value]
  (.uniformMatrix4fv (.-gl shader) (get (.-uniforms shader) uniform-name) false value)
  shader)

(defn shader-uniform-vector4-by-location
  [^Shader shader uniform-location value]
  (.uniform4fv (.-gl shader) uniform-location value)
  shader)

(defn shader-uniform-vector4
  [^Shader shader uniform-name value]
  (.uniform4fv (.-gl shader) (get (.-uniforms shader) uniform-name) value)
  shader)

(defn shader-uniform-int-by-location
  [^Shader shader uniform-location value]
  (.uniform1i (.-gl shader) uniform-location value)
  shader)

(defn shader-uniform-int
  [^Shader shader uniform-name value]
  (.uniform1i (.-gl shader) (get (.-uniforms shader) uniform-name) value)
  shader)

(defn use-shader
  [^Shader shader]
  (.useProgram (.-gl shader) (.-program shader)))

(defn- is-power-of-2
  [value]
  (= 0 (bit-and value (dec value))))

(defrecord Texture [gl gl-texture url width height])

(defn load-texture
  [gl url & {:keys [wrap wrap-s wrap-t
                    filter min-filter mag-filter]}]
  (go
    ;; convert options to their gl values
    (let [wrap-options {:clamp gl.CLAMP_TO_EDGE
                        :repeat gl.REPEAT
                        :mirrored-repeat gl.MIRRORED_REPEAT}
          mag-filter-options {:linear gl.LINEAR
                              :nearest gl.NEAREST}
          min-filter-options {:linear gl.LINEAR
                              :nearest gl.NEAREST
                              :nearest-mimap-nearest gl.NEAREST_MIPMAP_NEAREST
                              :linear-mipmap-nearest gl.LINEAR_MIPMAP_NEAREST
                              :nearest-mipmap-linear gl.NEAREST_MIPMAP_LINEAR
                              :linear-mipmap-linear gl.LINEAR_MIPMAP_LINEAR}
          wrap-s (or wrap-s wrap)
          wrap-t (or wrap-t wrap)
          min-filter (or min-filter filter)
          mag-filter (or mag-filter filter)
          wrap-s-value (get wrap-options wrap-s)
          wrap-t-value (get wrap-options wrap-t)
          min-filter-value (get min-filter-options min-filter)
          mag-filter-value (get mag-filter-options mag-filter)]
      (when (and wrap-s (not wrap-s-value))
        (throw (ex-info (str "Could not find value for wrap-s of " wrap-s) {})))
      (when (and wrap-t (not wrap-t-value))
        (throw (ex-info (str "Could not find value for wrap-t of " wrap-t) {})))
      (when (and min-filter (not min-filter-value))
        (throw (ex-info (str "Could not find value for min-filter of " min-filter) {})))
      (when (and mag-filter (not mag-filter-value))
        (throw (ex-info (str "Could not find value for mag-filter of " mag-filter) {})))

      (let [texture (.createTexture gl)]
        ;; create a temporary texture while the image loads
        (.bindTexture gl gl.TEXTURE_2D texture)
        (.texImage2D gl gl.TEXTURE_2D
                     0 ; level
                     gl.RGBA ; internal format
                     1 ; width
                     1 ; height
                     0 ; border
                     gl.RGBA ; source format
                     gl.UNSIGNED_BYTE ; source type
                     (js/Uint8Array. #js [0 0 255 255]))
        (let [image (<! (assetdb/load-asset url))]
          (.bindTexture gl gl.TEXTURE_2D texture)
          (.texImage2D
           gl
           gl.TEXTURE_2D
           0 ; level
           gl.RGBA ; internal format
           gl.RGBA ; source format
           gl.UNSIGNED_BYTE
           image)
          (if (and (is-power-of-2 (.-height image)) (is-power-of-2 (.-width image)))
            (.generateMipmap gl gl.TEXTURE_2D)
            (do (.texParameteri gl gl.TEXTURE_2D gl.TEXTURE_WRAP_S gl.CLAMP_TO_EDGE)
                (.texParameteri gl gl.TEXTURE_2D gl.TEXTURE_WRAP_T gl.CLAMP_TO_EDGE)))

          (when wrap-s-value
            (.texParameteri gl gl.TEXTURE_2D gl.TEXTURE_WRAP_S wrap-s-value))
          (when wrap-t-value
            (.texParameteri gl gl.TEXTURE_2D gl.TEXTURE_WRAP_T wrap-t-value))
          (when min-filter-value
            (.texParameteri gl gl.TEXTURE_2D gl.TEXTURE_MIN_FILTER min-filter-value))
          (when mag-filter-value
            (.texParameteri gl gl.TEXTURE_2D gl.TEXTURE_MAG_FILTER mag-filter-value))
          (map->Texture {:gl gl
                         :gl-texture texture
                         :width (.-width image)
                         :height (.-height image)
                         :url url}))))))

(defn activate-and-bind-texture
  [^Texture texture unit]
  (let [gl (.-gl texture)]
    (.activeTexture gl (+ gl.TEXTURE0 unit))
    (.bindTexture gl gl.TEXTURE_2D (.-gl-texture texture))
  texture))

(defn unbind-texture
  [^Texture texture]
  (let [gl (.-gl texture)]
    (.bindTexture gl gl.TEXTURE_2D nil))
  texture)


(defrecord Buffer [gl gl-buffer gl-buffer-type gl-data-type gl-draw-mode
                   data gl-data-create-fn buffer-type data-type draw-mode
                   element-size type-size stride attributes])
(defrecord AttributeInfo [location size offset])

(defn create-buffer
  [gl & {:keys [data-type buffer-type draw-mode] :or {data-type :float buffer-type :array draw-mode :triangles}}]
  (let [buffer (.createBuffer gl)
        gl-data-type (case data-type
                       :float gl.FLOAT
                       :int gl.INT
                       :uint gl.UNSIGNED_INT
                       :short gl.SHORT
                       :ushort gl.UNSIGNED_SHORT
                       :byte gl.BYTE
                       :ubyte gl.UNSIGNED_BYTE
                       (throw (ex-info "Unhandled data-type" {:data-type data-type})))
        type-size (case data-type
                    :float 4
                    :int 4
                    :uint 4
                    :short 2
                    :ushort 2
                    :byte 1
                    :ubyte 1
                    (throw (ex-info "Unhandled data-type" {:data-type data-type})))
        gl-buffer-type (case buffer-type
                         :array gl.ARRAY_BUFFER
                         :element-array gl.ELEMENT_ARRAY_BUFFER
                         (throw (ex-info "Unhandled buffer type" {:buffer-type buffer-type})))
        gl-draw-mode (case draw-mode
                       :triangles gl.TRIANGLES
                       :triangle-strip gl.TRIANGLE_STRIP
                       :lines gl.LINES
                       (throw (ex-info "Unhandled draw mode" {:draw-mode draw-mode})))
        gl-data-create-fn (case data-type
                            :float #(js/Float32Array. %)
                            :int #(js/Int32Array. %)
                            :uint #(js/Uint32Array. %)
                            :short #(js/Int16Array. %)
                            :ushort #(js/Uint16Array. %)
                            :byte #(js/Int8Array. %)
                            :ubyte #(js/Uint8Array. %)
                           (throw (ex-info "Unhandled data-type" {:data-type data-type})))]
    (map->Buffer {:gl gl
                  :gl-buffer buffer
                  :gl-buffer-type gl-buffer-type
                  :gl-data-type gl-data-type
                  :gl-draw-mode gl-draw-mode
                  :data []
                  :gl-data-create-fn gl-data-create-fn
                  :buffer-type buffer-type
                  :data-type data-type
                  :draw-mode draw-mode
                  :element-size 0
                  :type-size type-size
                  :stride 0
                  :attributes []})))

(defn add-buffer-data
  [^Buffer buffer data]
  (assoc buffer
         :data
         (reduce conj (:data buffer) data)))

(defn add-buffer-attribute
  [^Buffer buffer location size]
  (let [attribute (->AttributeInfo location size (.-element-size buffer))
        new-element-size (+ size (.-element-size buffer))
        new-stride (* new-element-size (.-type-size buffer))]
    (-> buffer
        (assoc :element-size new-element-size)
        (assoc :stride new-stride)
        (update :attributes #(conj % attribute)))))

(defn sync-buffer-data
  "Sync's buffer data to the gpu"
  [^Buffer buffer]
  (let [gl (.-gl buffer)
        gl-buffer (.-gl-buffer buffer)
        gl-buffer-type (.-gl-buffer-type buffer)
        gl-data-create-fn (.-gl-data-create-fn buffer)
        data (.-data buffer)
        gl-data (gl-data-create-fn (count data))]
    (doseq [idx (range (count data))]
      (aset gl-data idx (get data idx)))
    (.bindBuffer gl gl-buffer-type gl-buffer)
    (.bufferData gl gl-buffer-type gl-data gl.STATIC_DRAW))
  buffer)

(defn bind-buffer
  [^Buffer buffer]
  (let [gl (.-gl buffer)]
    (.bindBuffer gl (.-gl-buffer-type buffer) (.-gl-buffer buffer))
    (doseq [attribute (.-attributes buffer)]
      (.vertexAttribPointer gl
                            (.-location attribute)
                            (.-size attribute)
                            (.-gl-data-type buffer)
                            false
                            (.-stride buffer)
                            (* (.-offset attribute) (.-type-size buffer)))
      (.enableVertexAttribArray gl (.-location attribute))))
  buffer)

(defn unbind-buffer
  [^Buffer buffer]
  (let [gl (.-gl buffer)]
    (doseq [attribute (.-attributes buffer)]
      (.disableVertexAttribArray gl (.-location attribute)))
    (.bindBuffer gl (.-gl-buffer-type buffer) nil))
  buffer)

(defn draw-buffer
  ([^Buffer buffer]
   (case (.-buffer-type buffer)
     :array (.drawArrays (.-gl buffer)
                         (.-gl-draw-mode buffer)
                         0
                         (/ (count (.-data buffer)) (.-element-size buffer)))
     :element-array (.drawElements (.-gl buffer)
                                   (.-gl-draw-mode buffer)
                                   (count (.-data buffer))
                                   (.-gl-data-type buffer)
                                   0)))
  ([^Buffer buffer start count]
   (case (.-buffer-type buffer)
     :array (.drawArrays (.-gl buffer)
                         (.-gl-draw-mode buffer)
                         start
                         count)
     :element-array (.drawElements (.-gl buffer)
                                   (.-gl-draw-mode buffer)
                                   (count (.-data buffer))
                                   (.-gl-data-type buffer)
                                   0))))
