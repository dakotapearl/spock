package spockdataaccess.entity;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name="Metrics")
public class Metric implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    private String id;
    @NotNull
    protected String CodeURL;

    public Metric() {
    }

    public Metric(String id, String CodeURL) {
        this.id = id;
        this.CodeURL = CodeURL;
    }
    
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getCodeURL() {
        return CodeURL;
    }

    public void setCodeURL(String CodeURL) {
        this.CodeURL = CodeURL;
    }


    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Metric)) {
            return false;
        }
        Metric other = (Metric) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.Metric[ id=" + id + " ]";
    }
    
}
